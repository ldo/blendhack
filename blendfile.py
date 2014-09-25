#+
# Parsing of .blend files created by Blender <http://www.blender.org/>.
#
# For some info, see
#     "The Mystery of the Blend" <http://www.atmind.nl/blender/mystery_ot_blend.html>
#     The Blender source code, doc/blender_file_format subdirectory
#
# Further info not mentioned above:
#     Most blocks have code "DATA", however a few have special codes:
#     * always 1 block with code "GLOB", type FileGlobal
#     * 1 block with code "REND", type Link, but contents are actually the RenderInfo
#       struct in source/blender/blenloader/intern/writefile.c in the Blender source.
#       Looks like there is potential to have more than one of these in future, as
#       soon as some code is added that sets the R_BG_RENDER flag on scenes.
#     * 1 block with code "TEST", type Link, but contents are actually the preview image
#           bitmap, format is integer width, integer height, followed by width * height
#           32-bit RGBA pixels. For code that extracts it, see source file
#           release/bin/blender-thumbnailer.py.
#     * one block with code "WM\x00\x00", type wmWindowManager.
#     * blocks with code "SN\x00\x00" (newer Blender) or "SR\x00\x00" (older Blender), type bScreen.
#     * blocks with code "MC\x00\x00", type MovieClip.
#     * blocks with code "MS\x00\x00", type Mask.
#     * one or more blocks with code "SC\x00\x00", type Scene.
#     * blocks with code "CU\x00\x00", type Curve.
#     * blocks with code "MB\x00\x00", type MetaBall.
#     * blocks with code "IM\x00\x00", type Image.
#     * blocks with code "CA\x00\x00", type Camera.
#     * blocks with code "LA\x00\x00", type Lamp.
#     * blocks with code "LT\x00\x00", type Lattice.
#     * blocks with code "VF\x00\x00", type VectorFont.
#     * blocks with code "KE\x00\x00", type Key (shape key).
#     * blocks with code "WO\x00\x00", type World.
#     * blocks with code "TX\x00\x00", type Text.
#     * blocks with code "SK\x00\x00", type Speaker.
#     * blocks with code "SO\x00\x00", type Sound.
#     * blocks with code "GR\x00\x00", type Group.
#     * blocks with code "AC\x00\x00", type bAction.
#     * blocks with code "OB\x00\x00", type Object.
#     * blocks with code "MA\x00\x00", type Material.
#     * blocks with code "TE\x00\x00", type Tex(ture).
#     * blocks with code "ME\x00\x00", type Mesh.
#     * blocks with code "PA\x00\x00", type ParticleSettings.
#     * blocks with code "NT\x00\x00", type NodeTree.
#     * blocks with code "BR\x00\x00", type Brush.
#     * blocks with code "PY\x00\x00", type Script (obsolete).
#     * blocks with code "GD\x00\x00", type GreasePencil.
#     * blocks with code "IP\x00\x00", type Ipo (obsolete, replaced by FCurves in DATA blocks).
#     * blocks with code "LI\x00\x00", type Library.
#     * possibly one block with code "USER" (only present in startup.blend),
#       followed by DATA blocks containing custom keymaps, addon properties, autoexec paths
#       and style definitions.
#     * always 1 block with code "DNA1", containing the "structure DNA" (type definitions).
#       Must be the last block, except for "ENDB"; Blender actually stops reading the file
#       when it has processed this block.
#     * always 1 block with code "ENDB", marking the end of the file.
#     Also several "DATA" blocks specify a type of Link (dna_index = 0), which is 2 * ptrsize
#     bytes, but are smaller than this, or even way larger.
#     The above 2-letter codes may be found in source file source/blender/makesdna/DNA_ID.h.
#     The significance of the code always ending with two zero bytes is that these blocks
#     have user-visible names: these names are required to be unique among blocks of the
#     same type, and are prefixed internally with the two initial bytes of the block code,
#     allowing two blocks with different codes to have the same user-visible name.
#
# The GLOB block is the root of a directed acyclic graph by which all other blocks
# (except the REND and TEST blocks, and of course the DNA1 and ENDB blocks) are directly
# or indirectly referenced; UI elements are linked through its curscreen field (pointing
# to a bScreen structure), and scene/model data is linked through its curscene field
# (pointing to a Scene structure). bScreens and Scenes are each linked into their own
# doubly-linked list via next and prev fields. All other objects are found via pointers
# from these structures.
#
# Copyright 2012-2014 Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#-

import sys
import io
import struct
import gzip

#+
# Useful stuff
#-

def structread(fromfile, decode_struct) :
    """reads sufficient bytes from fromfile to be unpacked according to
    decode_struct, and returns the unpacked results."""
    return struct.unpack(decode_struct, fromfile.read(struct.calcsize(decode_struct)))
#end structread

class DanglingPointer :
    "for showing pointers that don't seem to point to any valid block"

    def __init__(self, addr) :
        self.addr = addr
    #end __init__

    def __repr__(self) :
        return "DanglingPointer(0x%x)" % self.addr
    #end __repr__

#end DanglingPointer

class RoutineAddr :
    "for showing pointers to methods"

    def __init__(self, addr) :
        self.addr = addr
    #end __init__

    def __repr__(self) :
        return "RoutineAddr(0x%x)" % self.addr
    #end __repr__

#end RoutineAddr

class BlockRef :
    "for showing references to other blocks"

    def __init__(self, block) :
        self.block = block
    #end __init__

    def __repr__(self) :
        return \
            "*Block[%d]" % self.block["index"]
    #end __repr__

#end BlockRef

#+
# Handling of Blender types
#-

class PointerType :
    "represents a pointer"

    def __init__(self, EltType) :
        self.EltType = EltType
    #end __init__

#end PointerType

class FixedArrayType :
    "represents a fixed-length array"

    def __init__(self, EltType, NrElts) :
        self.EltType = EltType
        self.NrElts = NrElts
    #end __init__

#end FixedArrayType :

class MethodType :
    "represents a pointer to a routine"

    def __init__(self, ResultType) :
        self.ResultType = ResultType
    #end __init__

#end MethodType

def parse_field_type(field_name, field_type) :
    # processes special forms of field_name ("*name", "name[ind]", "(*name)()"),
    # returning innermost name and adjusting field_type accordingly.
    if len(field_name) != 0 and field_name[0] == "*" :
        field_name, field_type = parse_field_type(field_name[1:], PointerType(field_type))
    elif field_name[:2] == "(*" and field_name[-3:] == ")()" :
        field_name, field_type = parse_field_type(field_name[2:-3], MethodType(field_type))
    else :
        indexstart = field_name.rfind("[")
        if indexstart >= 0 :
            assert field_name[-1] == "]", "%s does not end with ]" % repr(field_name)
            nr_elts = int(field_name[indexstart + 1 : -1])
            field_name, field_type = parse_field_type(field_name[:indexstart], field_type)
            field_type = FixedArrayType(field_type, nr_elts)
        #end if
    #end if
    return \
        field_name, field_type
#end parse_field_type

def encode_field_type(field_name, field_type) :
    # does the opposite of parse_field_type, encoding derived (pointer, array, method)
    # type information into field_name.
    if type(field_type) == PointerType :
        field_name, field_type = encode_field_type("*" + field_name, field_type.EltType)
    elif type(field_type) == MethodType :
        field_name, field_type = encode_field_type("(*%s)()" % field_name, field_type.ResultType)
    elif type(field_type) == FixedArrayType :
        orig_field_type = field_type
        field_name, field_type = encode_field_type(field_name, field_type.EltType)
        field_name = "%s[%d]" % (field_name, orig_field_type.NrElts)
    else :
        field_type = field_type["name"]
    #end if
    return \
        field_name, field_type
#end encode_field_type

def type_name(of_type) :
    # returns a readable display of of_type.
    return \
        {
            PointerType : lambda : type_name(of_type.EltType) + "*",
            FixedArrayType : lambda : "%s[%d]" % (type_name(of_type.EltType), of_type.NrElts),
            MethodType : lambda : "%s (*)()" % type_name(of_type.ResultType),
        }.get(type(of_type), lambda : of_type["name"])()
#end type_name

primitive_types = \
    { # "code" is the code to pass to struct.pack/unpack to pack/unpack values of this type
        "char" : {"code" : "c", "size" : 1},
        "uchar" : {"code" : "B", "size" : 1},
        "short" : {"code" : "h", "size" : 2},
        "ushort" : {"code" : "H", "size" : 2},
        "int" : {"code" : "i", "size" : 4},
        # notice what's missing? No unsigned int!
        "long" : {"code" : "l", "size" : 4},
        "ulong" : {"code" : "L", "size" : 4},
        "float" : {"code" : "f", "size" : 4},
        "double" : {"code" : "d", "size" : 8},
        "int64_t" : {"code" : "q", "size" : 8}, # might not be present
        "uint64_t" : {"code" : "Q", "size" : 8}, # might not be present
        "void" : {"size" : 0}, # only occurs as pointer object type!
    }

block_code_order = \
    ( # blocks should be written to file in this order to avoid Blender crashes
        b"WM\x00\x00", # type wmWindowManager.
        b"SN\x00\x00", # (newer Blender) type bScreen.
        b"SR\x00\x00", # (older Blender) type bScreen.
        b"MC\x00\x00", # type MovieClip.
        b"MS\x00\x00", # type Mask.
        b"SC\x00\x00", # type Scene.
        b"CU\x00\x00", # type Curve.
        b"MB\x00\x00", # type MetaBall.
        b"IM\x00\x00", # type Image.
        b"CA\x00\x00", # type Camera.
        b"LA\x00\x00", # type Lamp.
        b"LT\x00\x00", # type Lattice.
        b"VF\x00\x00", # type VectorFont.
        b"KE\x00\x00", # type Key (shape key).
        b"WO\x00\x00", # type World.
        b"TX\x00\x00", # type Text.
        b"SK\x00\x00", # type Speaker.
        b"SO\x00\x00", # type Sound.
        b"GR\x00\x00", # type Group.
        b"AC\x00\x00", # type bAction.
        b"OB\x00\x00", # type Object.
        b"MA\x00\x00", # type Material.
        b"TE\x00\x00", # type Tex(ture).
        b"ME\x00\x00", # type Mesh.
        b"PA\x00\x00", # type ParticleSettings.
        b"NT\x00\x00", # type NodeTree.
        b"BR\x00\x00", # type Brush.
        b"PY\x00\x00", # type Script (obsolete).
        b"GD\x00\x00", # type GreasePencil.
        b"IP\x00\x00", # type Ipo (obsolete, replaced by FCurves).
        b"LI\x00\x00", # type Library.
    )

blender_sig = b"BLENDER"

class Blenddata :
    "Call the load method on an instance of this to parse a .blend file, passing it" \
    " the pathname of the file to read and parse. The returned object will contain the" \
    " parsed contents of the file."

    # Instance variables set up by load and decode_sdna routines:
    #     blocks --
    #         list of blocks from file, in order of appearance
    #     blocks_by_oldaddress --
    #         table of blocks from file, indexed by old-address field
    #     endian --
    #         prefix code to use with struct.unpack to indicate endianness of data
    #     global_block --
    #         "GLOB" block
    #     link_type --
    #         the Link type, that needs to be treated specially because fields
    #         of this type are often missing
    #     ptrcode --
    #         code to use to struct.unpack to decode a pointer field
    #     ptrsize --
    #         size in bytes of an address according to version of Blender which created the file
    #     structs_by_index --
    #         struct type definitions collected from Structure DNA block, indexed by number
    #     types --
    #         type definitions collected from Structure DNA block, indexed by name
    #     types_by_index
    #         type definitions collected from Structure DNA block, indexed by number
    #     version --
    #         3-character version code from file header

    # Struct (non-primitive) types will be found in structs_by_index, types and types_by_index;
    # primitive types only in the latter two. Non-pointer/array/method types are represented
    # by dicts with the following entries:
    #     "fields" -- (non-primitive types only) array of field definitions, each of which
    #                 is a dict, with "name" and "type" entries
    #     "index" -- the index of the type entry in types_by_index
    #     "name" -- the (string) name of the type
    #     "size" -- the total size in bytes of the type
    # Pointer, array and method types are represented by objects of PointerType,
    # FixedArrayType and MethodType (defined above) respectively.

    # Each block is represented by a dict with the following entries:
    #     "code" -- the four-byte block code
    #     "data" -- (only if the block contents were decodeable) the decoded block data, as an array of elements of the block type
    #     "decoded" -- indicates that the block contents were decodeable. False only for special-case blocks
    #     "dna_count" -- the number of array elements in the block
    #     "dna_index" -- the index into structs_by_index of the block type
    #     "endaddr" -- one past the end of the in-memory addresses spanned by the block
    #     "index" -- the index number of the block, used for less-cluttered display dumps
    #     "oldaddr" -- the saved in-memory address of the block
    #     "rawdata" -- the raw, undecoded data (only kept if keep_rawdata is specified to load)
    #     "refs" -- number of references to this block (optional, only present if count_refs is specified to load)
    #     "type" -- (only if the block contents were decodeable) -- the reference to the structure type of the block contents

    def __init__(self, big_endian = None, pointer_size = None) :
        "args are only important for writing, ignored for reading"
        if big_endian == None :
            big_endian = sys.byteorder == "big"
        #end if
        if pointer_size == None :
            pointer_size = 4 # given I'm not really using them as pointers, why waste space?
        #end if
        self.ptrsize = pointer_size
        self.ptrcode = {4 : "L", 8 : "Q"}[pointer_size]
        self.big_endian = big_endian
        self.endian = {False : "<", True : ">"}[big_endian]
    #end __init__

    def type_size(self, of_type) :
        # determines the size in bytes of objects of type of_type.
        if type(of_type) == PointerType :
            result = self.ptrsize
        elif type(of_type) == FixedArrayType :
            result = self.type_size(of_type.EltType) * of_type.NrElts
        elif type(of_type) == MethodType :
            result = self.ptrsize
        else :
            result = of_type["size"]
        #end if
        return \
            result
    #end type_size

    def decode_sdna(self, sdna_data, log) :
        "decodes a structure definitions block and saves the results in instance variables."
        sdna_id = sdna_data[:4]
        assert sdna_id == b"SDNA", "invalid DNA block header"
        sdna_data = sdna_data[4:]
        names = []
        self.types_by_index = []
        self.structs_by_index = []
        self.link_type = None
        data_offset = 4
        for \
            expect_id, collect \
        in \
            (
                (b"NAME", names),
                (b"TYPE", self.types_by_index),
            ) \
        :
            assert len(sdna_data) >= 8, "premature end of DNA block"
            assert expect_id == sdna_data[:4], "expecting %s sub-block in DNA block" % expect_id
            nr_items = struct.unpack(self.endian + "I", sdna_data[4:8])[0]
            sdna_data = sdna_data[8:]
            data_offset += 8
            for i in range(nr_items) :
                str_end = sdna_data.index(b"\0")
                collect.append(sdna_data[:str_end].decode("utf-8"))
                log.write("name[%d] = %s\n" % (i, repr(collect[i]))) # debug
                data_offset += str_end + 1
                sdna_data = sdna_data[str_end + 1:]
            #end for
            if data_offset % 4 != 0 :
                sdna_data = sdna_data[4 - data_offset % 4:]
                data_offset += 4 - data_offset % 4
            #end if
        #end for
        for i in range(len(self.types_by_index)) :
            self.types_by_index[i] = {"name" : self.types_by_index[i], "index" : i}
        #end for
        assert sdna_data[:4] == b"TLEN", "expecting TLEN sub-block in DNA block"
        sdna_data = sdna_data[4:]
        data_offset += 4
        for \
            i, s \
        in \
            enumerate \
              (
                struct.unpack
                  (
                        self.endian
                    +
                        "H" * len(self.types_by_index),
                    sdna_data[:2 * len(self.types_by_index)]
                  )
              ) \
        :
            self.types_by_index[i]["size"] = s
            log.write("sizeof(%s) = %d\n" % (self.types_by_index[i]["name"], self.types_by_index[i]["size"])) # debug
        #end for
        sdna_data = sdna_data[2 * len(self.types_by_index):]
        data_offset += 2 * len(self.types_by_index)
        if data_offset % 4 != 0 :
            sdna_data = sdna_data[4 - data_offset % 4:]
            data_offset += 4 - data_offset % 4
        #end if
        assert sdna_data[:4] == b"STRC", "expecting STRC sub-block in DNA block"
        nr_structs = struct.unpack(self.endian + "I", sdna_data[4:8])[0]
        sdna_data = sdna_data[8:]
        data_offset += 8
        for i in range(nr_structs) :
            struct_type, nr_fields = struct.unpack(self.endian + "HH", sdna_data[:4])
            sdna_data = sdna_data[4:]
            fields = []
            for \
                j, f \
            in \
                enumerate(struct.unpack(self.endian + "HH" * nr_fields, sdna_data[:4 * nr_fields])) \
            :
                if j % 2 == 0 :
                    field_type = f
                else :
                    field_name, field_type = parse_field_type(names[f], self.types_by_index[field_type])
                    fields.append({"type" : field_type, "name" : field_name})
                    log.write("%s.%s : %s\n" % (self.types_by_index[struct_type]["name"], field_name, type_name(field_type))) # debug
                #end if
            #end for
            self.types_by_index[struct_type]["fields"] = fields
            if len(self.structs_by_index) < i + 1 :
                self.structs_by_index.extend([None] * (i + 1 - len(self.structs_by_index)))
            #end if
            self.structs_by_index[i] = self.types_by_index[struct_type]
            log.write("struct[%d] is %s\n" % (i, self.types_by_index[struct_type]["name"])) # debug
            sdna_data = sdna_data[4 * nr_fields:]
        #end for
        self.types = {}
        for t in self.types_by_index :
            assert (t["name"] in primitive_types) <= ("fields" not in t), "primitive type %s must not be struct" % t["name"]
            self.types[t["name"]] = t
        #end for
        self.link_type = self.types["Link"]
          # should I bother to check it consists of exactly 2 fields, both of type *Link?
        for k in primitive_types :
            assert k not in self.types or primitive_types[k]["size"] == self.types[k]["size"], "wrong type size for primitive type %s, expected %d, got %d" % (k, primitive_types[k]["size"], self.types[k]["size"])
        #end for
    #end decode_sdna

    def encode_sdna(self) :
        "returns bytes for an SDNA block representing my defined types."
        names = {} # mapping of name strings to indexes
        structs_by_index = []
        for this_struct in self.structs_by_index :
            fields = []
            for field in this_struct["fields"] :
                field_name, field_type = encode_field_type(field["name"], field["type"])
                if not field_name in names :
                    names[field_name] = len(names)
                #end if
                fields.append \
                  (
                    {
                        "name" : names[field_name],
                        "type" : self.types[field_type]["index"],
                    }
                  )
            #end for
            structs_by_index.append \
              (
                {
                    "type" : this_struct["index"],
                    "fields" : fields,
                }
              )
        #end for
        out = io.BytesIO()
        out.write(b"SDNA")
        for \
            subblock_id, contents \
        in \
            (
                (b"NAME", sorted(names.keys(), key = lambda n : names[n])),
                (b"TYPE", (thistype["name"] for thistype in self.types_by_index)),
            ) \
        :
            contents = list(contents) # need length
            out.write(subblock_id + struct.pack(self.endian + "I", len(contents)))
            offset = 0
            for thisname in contents :
                thisname = thisname.encode("utf-8") + b"\0"
                out.write(thisname)
                offset += len(thisname)
            #end for
            if offset % 4 != 0 :
                out.write(b"\0" * (4 - offset % 4))
            #end if
        #end for
        out.write(b"TLEN")
        offset = 0
        for thistype in self.types_by_index:
            out.write(struct.pack(self.endian + "H", thistype["size"]))
            offset += 2
        #end for
        if offset % 4 != 0 :
            out.write(b"\0" * (4 - offset % 4))
        #end if
        out.write(b"STRC" + struct.pack(self.endian + "I", len(structs_by_index)))
        for this_struct in structs_by_index :
            fields = this_struct["fields"]
            out.write(struct.pack(self.endian + "HH", this_struct["type"], len(fields)))
            for field in fields :
                out.write(struct.pack(self.endian + "HH", field["type"], field["name"]))
            #end for
        #end for
        return \
            out.getvalue()
    #end encode_sdna

    def decode_data(self, rawdata, datatype, log) :
        # decodes the bytes of rawdata to Python form according to the type datatype.
        if type(datatype) == PointerType :
            assert len(rawdata) == self.ptrsize, "expecting pointer to be %d bytes, got %d" % (self.ptrsize, len(rawdata))
            oldaddress = struct.unpack(self.endian + self.ptrcode, rawdata)[0]
            if oldaddress == 0 :
                result = None
            elif oldaddress in self.blocks_by_oldaddress :
                # result = self.blocks_by_oldaddress[oldaddress]
                the_block = self.blocks_by_oldaddress[oldaddress]
                if "refs" in the_block :
                    the_block["refs"] += 1
                #end if
                result = BlockRef(the_block)
            else :
                result = DanglingPointer(oldaddress)
            #end if
        elif type(datatype) == FixedArrayType :
            result = []
            elt_size = self.type_size(datatype.EltType)
            assert len(rawdata) == elt_size * datatype.NrElts
            for i in range(datatype.NrElts) :
                result.append(self.decode_data(rawdata[i * elt_size : (i + 1) * elt_size], datatype.EltType, log))
            #end for
            if datatype.EltType == self.types["char"] :
                # prettier display
                result = b"".join(result).rstrip(b"\0")
                try :
                    result = result.decode("utf-8")
                except UnicodeDecodeError :
                    pass # leave as bytes
                #end try
            #end if
        elif type(datatype) == MethodType :
            assert len(rawdata) == self.ptrsize, "expecting method pointer to be %d bytes, got %d" % (self.ptrsize, len(rawdata))
            addr = struct.unpack(self.endian + self.ptrcode, rawdata)[0]
            if addr != 0 :
                result = RoutineAddr(addr)
                  # show value just out of curiosity, saved value on disk can't possibly be useful
            else :
                result = None
            #end if
        elif datatype["name"] in primitive_types :
            assert len(rawdata) == primitive_types[datatype["name"]]["size"], "wrong size for primitive data of type %s, expected %d, got %d" % (datatype["name"], primitive_types[datatype["name"]]["size"], len(rawdata))
            result = struct.unpack(self.endian + primitive_types[datatype["name"]]["code"], rawdata)[0]
        else :
            assert "fields" in datatype, "non-primitive type %s has no fields" % datatype["name"]
            result = {}
            for field in datatype["fields"] :
                field_size = self.type_size(field["type"])
                if len(rawdata) < field_size :
                    log.write("# need at least %d bytes for %s.%s, only got %d\n" % (field_size, datatype["name"], field["name"], len(rawdata)))
                    rawdata = b"" # ignore rest
                    break
                #end if
                result[field["name"]] = self.decode_data(rawdata[:field_size], field["type"], log)
                rawdata = rawdata[field_size:]
            #end for
            assert len(rawdata) == 0, "leftover data after decoding %s struct" % datatype["name"]
        #end if
        return \
            result
    #end decode_data

    default_encode_ref = lambda block : block["oldaddr"]

    def encode_data(self, block, encode_ref = default_encode_ref) :
        # encodes the specified block contents to raw data and returns it.

        def encode_item(data, datatype) :
            if type(datatype) == PointerType :
                # oldaddress isn't actually important
                if data == None :
                    data = 0
                elif type(data) == DanglingPointer :
                    data = data.addr
                      # uniqueness of most values need to be preserved, to keep Blender from
                      # crashing. Some of these, it seems, can be safely set to 0--in a file
                      # created by 64-bit Blender, these have addresses within the first 1GB.
                      # However, to be safe, I just preserve them all.
                elif type(data) == BlockRef :
                    data = encode_ref(data.block)
                else :
                    raise AssertionError("decoded pointer value not None, BlockRef or DanglingPointer")
                #end if
                result = struct.pack(self.endian + self.ptrcode, data)
            elif type(datatype) == FixedArrayType :
                if type(data) == bytes :
                    result = data
                elif type(data) == str :
                    result = data.encode("utf-8")
                else :
                    result = b"".join \
                      (
                        encode_item(i, datatype.EltType) for i in data
                      )
                #end if
                if datatype.EltType == self.types["char"] :
                    # put back any trailing nulls I might have removed
                    result += b"\0" * (datatype.NrElts - len(result))
                #end if
            elif type(datatype) == MethodType :
                # oldaddress can't possibly be useful
                result = struct.pack(self.endian + self.ptrcode, 0)
            elif datatype["name"] in primitive_types :
                result = struct.pack(self.endian + primitive_types[datatype["name"]]["code"], data)
            else :
                assert "fields" in datatype, "non-primitive type %s has no fields" % datatype["name"]
                result = b""
                for field in datatype["fields"] :
                    result += encode_item \
                      (
                        (data.__getitem__, data.get)[datatype == self.link_type](field["name"]),
                        field["type"]
                      )
                #end for
            #end if
            return \
                result
        #end encode_item

    #begin encode_data
        return \
            (
                lambda : block["rawdata"],
                lambda : b"".join \
                  (
                    encode_item(i, self.structs_by_index[block["dna_index"]]) for i in block["data"]
                  ),
            )[block["decoded"]]()
    #end encode_data

    def construct_block(self, code, oldaddr, dna_index, dna_count, contents) :
        # prepends a header for the specified block contents and returns the complete block.
        return \
            (
                struct.pack
                  (
                    "%s4sI%sII" % (self.endian, self.ptrcode),
                    code,
                    len(contents),
                    oldaddr,
                    dna_index,
                    dna_count,
                  )
            +
                contents
            )
    #end construct_block

    def dump_counts(self, log) :
        # debug--dumps some stats about block types.
        block_codes = {}
        block_types = {}
        block_codes_unrefd = None
        block_types_unrefd = None
        max_name_length = 0
        for block in self.blocks :
            if block["decoded"] :
                this_code = block["code"]
                this_type = block["type"]["name"]
                max_name_length = max(max_name_length, len(this_type))
                block_codes[this_code] = block_codes.get(this_code, 0) + 1
                block_types[this_type] = block_types.get(this_type, 0) + 1
                if "refs" in block :
                    if block_codes_unrefd == None :
                        block_codes_unrefd = {}
                    #end if
                    if block_types_unrefd == None :
                        block_types_unrefd = {}
                    #end if
                    if block["refs"] == 0 :
                        block_codes_unrefd[this_code] = block_codes_unrefd.get(this_code, 0) + 1
                        block_types_unrefd[this_type] = block_types_unrefd.get(this_type, 0) + 1
                    #end if
                #end if
            #end if
        #end for
        log.write("\nBlock code counts:\n")

        for this_code in sorted(block_codes) :
            unrefd = block_codes_unrefd.get(this_code, 0)
            log.write \
              (
                    (
                        "    %(code)-19s %(count)d"
                    +
                        ("", " (unref’d: %(unrefd)d)")[unrefd != 0]
                    +
                        "\n"
                    )
                %
                    {
                        "code" : repr(this_code),
                        "count" : block_codes[this_code],
                        "unrefd" : unrefd,
                    }
              )
        #end if
        log.write("Block type counts:\n")
        for this_type in sorted(block_types) :
            unrefd = block_types_unrefd.get(this_type, 0)
            log.write \
              (
                    (
                        "    %%(type)-%ds %%(count)d" % max_name_length
                    +
                        ("", " (unref’d: %(unrefd)d)")[unrefd != 0]
                    +
                        "\n"
                    )
                %
                    {
                        "type" : this_type,
                        "count" : block_types[this_type],
                        "unrefd" : unrefd,
                    }
              )
        #end for
    #end dump_counts

    def load(self, filename, keep_rawdata = False, count_refs = False, log = None) :
        "loads the contents of the specified .blend file."
        openlog = None
        if log == None :
            openlog = open("/dev/null", "w")
            log = openlog
        #end if
        origfd = open(filename, "rb")
        sig = origfd.read(2)
        origfd.seek(0)
        if sig == b"\x1F\x8B" :
            fd = gzip.GzipFile(mode = "r", fileobj = origfd)
        else :
            fd = origfd
            origfd = None
        #end if
        sig, ptrcode, endiancode, self.version = structread(fd, "7s1s1s3s") # note not endian-dependent
        assert sig == blender_sig, "unrecognized file header signature %s" % sig
        self.ptrsize = {b"_" : 4, b"-" : 8}[ptrcode]
        self.ptrcode = {b"_" : "L", b"-" : "Q"}[ptrcode]
        self.endian = {b"v" : "<", b"V" : ">"}[endiancode]
        self.big_endian = endiancode == "V"
        log.write("File Blender version = %s, ptrsize = %d, endian = %s\n" % (repr(self.version), self.ptrsize, ("little", "big")[endiancode == "V"]))
        self.blocks = []
        self.blocks_by_oldaddress = {}
        self.global_block = None
        sdna_seen = False
        while True :
            # collect all the blocks
            blockcode, datasize, oldaddr, dna_index, dna_count = \
                structread(fd, "%s4sI%sII" % (self.endian, self.ptrcode))
            log.write("blockcode at 0x%08x = %s, datasize = %d, oldaddr = 0x%x, dna_index = %d, dna_count = %d\n" % (fd.tell(), blockcode, datasize, oldaddr, dna_index, dna_count)) # debug
            # log.write("blockcode = %s, datasize = %d, oldaddr = 0x%x, dna_index = %d, dna_count = %d\n" % (blockcode, datasize, oldaddr, dna_index, dna_count)) # debug
            if blockcode == b"DNA1" :
                assert not sdna_seen, "duplicate SDNA blocks"
                self.decode_sdna(fd.read(datasize), log)
                sdna_seen = True
            elif blockcode == b"ENDB" :
                # file-end marker block
                break
            else :
                assert not sdna_seen, "data blocks after SDNA block"
                new_block = \
                    {
                        "code" : blockcode,
                        "oldaddr" : oldaddr,
                        "endaddr" : oldaddr + datasize,
                        "dna_index" : dna_index,
                        "dna_count" : dna_count,
                        "rawdata" : fd.read(datasize),
                        "index" : len(self.blocks),
                    }
                if count_refs :
                    new_block["refs"] = 0
                #end if
                if blockcode == b"GLOB" :
                    assert self.global_block == None, "multiple GLOB blocks found"
                    self.global_block = new_block
                #end if
                self.blocks.append(new_block)
                self.blocks_by_oldaddress[new_block["oldaddr"]] = new_block
            #end if
        #end while
        assert sdna_seen, "missing SDNA block"
        fd.close()
        if origfd != None :
            origfd.close()
        #end if
        assert self.global_block != None, "missing GLOB block"
        for i, block in enumerate(self.blocks) :
            block_type = self.structs_by_index[block["dna_index"]]
            block["decoded"] = \
                (
                    block["code"] not in (b"REND", b"TEST")
                and
                    (block_type != self.link_type or len(block["rawdata"]) == block_type["size"])
                      # there are some blocks of Link type that are too small for the type
                )
            if block["decoded"] :
                block["type"] = block_type
                decoded = []
                for j in range(block["dna_count"]) :
                    decoded.append \
                      (
                        self.decode_data
                          (
                            block["rawdata"][j * block_type["size"] : (j + 1) * block_type["size"]],
                            block_type,
                            log
                          )
                      )
                #end for
                leftover = max(len(block["rawdata"]) - block_type["size"] * block["dna_count"], 0)
                if leftover > 0 :
                    log.write("# ignoring %d bytes at end of block %d\n" % (leftover, i))
                #end if
                block["data"] = decoded
                if not keep_rawdata :
                    del block["rawdata"]
                #end if
            #end if
        #end for
        self.dump_counts(log) # debug
        if openlog != None :
            openlog.flush()
            openlog.close()
        #end if
        return \
            self
    #end load

    def save(self, filename, compressed = False, encode_ref = default_encode_ref, use_rawdata = False, log = None) :
        "saves the contents into a .blend file."

        referenced = set() # set of blocks that should be written out

        def find_referenced() :
            # marks all specially-coded blocks needing saving.

            def referenced_action(block) :
                if block["code"] != b"DATA" :
                    referenced.add(encode_ref(block))
                #end if
                return \
                    True
            #end referenced_action

        #begin find_referenced
            scan_block(self.global_block, referenced_action)
        #end find_referenced

        def scan_block(block, action) :
            # invokes action on the specified block, followed by all (indirectly or directly)
            # referenced blocks.

            scanned = set() # blocks that have already been scanned

            def scan_block_recurse(block, action) :
                # invokes action on the specified block, followed by all (indirectly or directly)
                # referenced blocks that haven't already been scanned.

                def check_item(item, itemtype) :

                    def check_scan_ref() :
                        # assume at most one level of pointer indirection!
                        if type(item) == BlockRef :
                            scan_block_recurse(item.block, action)
                        #end if
                    #end check_scan_ref

                    def check_array() :
                        # debug
                        if len(item) != itemtype.NrElts :
                            log.write("array %s has %d elts, expected %d\n" % (repr(item), len(item), itemtype.NrElts))
                        #end if
                        #end debug
                        for i in range(itemtype.NrElts) :
                            check_item(item[i], itemtype.EltType)
                        #end for
                    #end check_array

                    def check_struct() :
                        for field in itemtype["fields"] :
                            fieldval = (item.__getitem__, item.get)[itemtype == self.link_type](field["name"])
                            if fieldval != None :
                                check_item(fieldval, field["type"])
                            #end if
                        #end for
                    #end check_struct

                #begin check_item
                    # transitively check other referenced blocks that haven't already
                    # been scanned
                    if type(itemtype) == PointerType :
                        check_scan_ref()
                    elif type(itemtype) == FixedArrayType and itemtype.EltType != self.types["char"] :
                        check_array()
                    elif type(itemtype) == dict and "fields" in itemtype :
                        check_struct()
                    #end if
                #end check_item

            #begin scan_block_recurse
                # debug
                if encode_ref(block) in scanned :
                    log.write("already scanned block %d\n" % block["index"])
                #end debug
                if encode_ref(block) not in scanned :
                    if action(block) :
                        scanned.add(encode_ref(block))
                        if block["decoded"] :
                            check_item \
                              (
                                block["data"],
                                FixedArrayType
                                  (
                                    self.structs_by_index[block["dna_index"]],
                                    len(block["data"])
                                  )
                              )
                        #end if
                    #end if
                #end if
            #end scan_block_recurse

        #begin scan_block
            scan_block_recurse(block, action)
        #end scan_block

        def save_block(block) :
            # saves the specified block, followed by all (indirectly or directly)
            # referenced blocks that haven't already been saved.

            context = \
                {
                    "done_coded" : False,
                }

            def save_action(block) :
                doit = block["code"] == b"DATA" or not context["done_coded"]
                if doit :
                    log.write("save block %d\n" % block["index"]) # debug
                    if block["code"] != b"DATA" :
                        context["done_coded"] = True
                    #end if
                    outfile.write \
                      (
                        self.construct_block
                          (
                            block["code"],
                            encode_ref(block),
                            block["dna_index"],
                            block["dna_count"],
                            (
                                lambda : self.encode_data(block, encode_ref = encode_ref),
                                lambda : block["rawdata"],
                            )[not block["decoded"] or use_rawdata and "rawdata" in block]()
                          )
                      )
                #end if
                return \
                    doit
            #end save_action

        #begin save_block
            scan_block(block, save_action)
        #end save_block

    #begin save
        openlog = None
        if log == None :
            openlog = open("/dev/null", "w")
            log = openlog
        #end if
        # self.ptrsize = 4; self.ptrcode = "L" # hack!
        outfile = (open, gzip.open)[compressed](filename, "wb")
        outfile.write \
          (
                blender_sig
            +
                {4 : b"_", 8 : b"-"}[self.ptrsize]
            +
                {False : b"v", True : b"V"}[self.big_endian]
            +
                self.version
          )
        find_referenced()
        save_block(self.global_block) # global block comes first
        for code in block_code_order :
            for block in self.blocks :
                if block["code"] == code and encode_ref(block) in referenced :
                    save_block(block)
                #end if
            #end for
        #end for
        outfile.write \
          (
            self.construct_block(b"DNA1", 0, 0, 1, self.encode_sdna())
          )
        outfile.write \
          (
            self.construct_block(b"ENDB", 0, 0, 0, b"")
          )
        outfile.flush()
        outfile.close()
        if openlog != None :
            openlog.flush()
            openlog.close()
        #end if
        return \
            self
    #end save

#end Blenddata
