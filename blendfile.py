#+
# Parsing of .blend files created by Blender <http://www.blender.org/>.
#
# For some info, see
#     "The Mystery of the Blend" <http://www.atmind.nl/blender/mystery_ot_blend.html>
#     The Blender source code, doc/blender_file_format subdirectory
#
# Further info not mentioned above:
#
# Most blocks have code "DATA", however a few have special codes:
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
#     * blocks with code "SC\x00\x00", type Scene.
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
#     * blocks with code "AR\x00\x00", type bArmature.
#     * blocks with code "AC\x00\x00", type bAction.
#     * blocks with code "OB\x00\x00", type Object.
#     * blocks with code "MA\x00\x00", type Material.
#     * blocks with code "TE\x00\x00", type Tex(ture).
#     * blocks with code "ME\x00\x00", type Mesh.
#     * blocks with code "PA\x00\x00", type ParticleSettings.
#     * blocks with code "NT\x00\x00", type NodeTree.
#     * blocks with code "BR\x00\x00", type Brush.
#     * blocks with code "PL\x00\x00", type Palette.
#     * blocks with code "PC\x00\x00", type PaintCurve.
#     * blocks with code "PY\x00\x00", type Script (obsolete).
#     * blocks with code "GD\x00\x00", type GreasePencil.
#     * blocks with code "IP\x00\x00", type Ipo (obsolete, replaced by FCurves in DATA blocks).
#     * blocks with code "LS\x00\x00", type FreestyleLineStyle.
#     * blocks with code "LI\x00\x00", type Library.
#     * blocks with code "CF\x00\x00", type CacheFile.
#     * possibly one block with code "USER" (only present in startup.blend),
#       followed by DATA blocks containing custom keymaps, addon properties, autoexec paths
#       and style definitions.
#     * always 1 block with code "DNA1", containing the "structure DNA" (struct type definitions).
#       Must be the last block, except for "ENDB"; Blender actually stops reading the file
#       when it has processed this block. The first type defined here, with index 0, is
#       always of type Link.
#     * always 1 block with code "ENDB", marking the end of the file. This may not be
#       a complete block; the dna_index and dna_count fields might be missing.
#
# Also several "DATA" blocks specify a type of Link (dna_index = 0), which is 2 * ptrsize
# bytes, but are not actually of this type, and can be smaller than this, or even way larger.
#
# The above 2-letter codes may be found in source file source/blender/makesdna/DNA_ID.h.
# The significance of the code always ending with two zero bytes is that these blocks
# have user-visible names: these names are required to be unique among blocks of the
# same type, and are prefixed internally with the two initial bytes of the block code,
# allowing two blocks with different codes to have the same user-visible name.
#
# The GLOB block is the root of a directed acyclic graph by which all other blocks
# (except the REND, TEST and USER blocks, and of course the DNA1 and ENDB blocks) are directly
# or indirectly referenced; UI elements are linked through its curscreen field (pointing
# to a bScreen structure), and scene/model data is linked through its curscene field
# (pointing to a Scene structure). bScreens and Scenes are each linked into their own
# doubly-linked list via next and prev fields. All other objects are found via pointers
# from these structures.
#
# Copyright 2012-2017 Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
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

MAX_ID_NAME = 66 # from source/blender/makesdna/DNA_ID.h

class PointerType :
    "represents a pointer"

    def __init__(self, EltType) :
        self.EltType = EltType
    #end __init__

    def __repr__(self) :
        return \
            "*%s" % repr(self.EltType)
    #end __repr__

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
    # processes special forms of field_name ("*name", "name[ind]", "(*name)()", "(*name)(void)"),
    # returning innermost name and adjusting field_type accordingly.
    if len(field_name) != 0 and field_name[0] == "*" :
        field_name, field_type = parse_field_type(field_name[1:], PointerType(field_type))
    elif field_name[:2] == "(*" and field_name[-3:] == ")()" :
        field_name, field_type = parse_field_type(field_name[2:-3], MethodType(field_type))
    elif field_name[:2] == "(*" and field_name[-7:] == ")(void)" :
        field_name, field_type = parse_field_type(field_name[2:-7], MethodType(field_type))
          # note I don't remember whether I saw "(void)" or "()"
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

def is_primitive_type(of_type) :
    return \
        type(of_type) == dict and of_type["name"] in primitive_types
#end is_primitive_type

def make_primitive_type(name) :
    entry = primitive_types[name]
    return \
        {
            "name" : name,
            "size" : primitive_types[name]["size"],
        }
#end make_primitive_type

def is_struct_type(of_type) :
    return \
        type(of_type) == dict and "fields" in of_type
#end is_struct_type

def is_string_type(of_type) :
    return \
        type(of_type) == FixedArrayType and type(of_type.EltType) == dict and of_type.EltType["name"] == "char"
#end is_string_type

def align_adjust(offset, multiple) :
    "returns amount to add to offset [0 .. multiple - 1] to" \
    " ensure it can be divided by multiple exactly."
    return \
        (multiple - offset % multiple) % multiple
#end align_adjust

block_code_order = \
    ( # blocks should be written to file in this order to avoid Blender crashes.
      # order taken from write_file_handle routine in source/blender/blenloader/intern/writefile.c.
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
        b"IK\x00\x00", # type Ika (obsolete).
        b"LT\x00\x00", # type Lattice.
        b"VF\x00\x00", # type VectorFont.
        b"KE\x00\x00", # type Key (shape key).
        b"WO\x00\x00", # type World.
        b"TX\x00\x00", # type Text.
        b"SK\x00\x00", # type Speaker.
        b"SO\x00\x00", # type Sound.
        b"GR\x00\x00", # type Group.
        b"AR\x00\x00", # type bArmature
        b"AC\x00\x00", # type bAction.
        b"OB\x00\x00", # type Object.
        b"MA\x00\x00", # type Material.
        b"TE\x00\x00", # type Tex(ture).
        b"ME\x00\x00", # type Mesh.
        b"PA\x00\x00", # type ParticleSettings.
        b"NT\x00\x00", # type NodeTree.
        b"BR\x00\x00", # type Brush.
        b"PL\x00\x00", # type Palette.
        b"PC\x00\x00", # type PaintCurve.
        b"PY\x00\x00", # type Script (obsolete).
        b"GD\x00\x00", # type GreasePencil.
        b"IP\x00\x00", # type Ipo (obsolete, replaced by FCurves).
        b"LS\x00\x00", # type FreestyleLineStyle
        b"CF\x00\x00", # type CacheFile
        b"LI\x00\x00", # type Library.
    )

blender_sig = b"BLENDER"
  # file must begin with this

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
    #     renderinfo_block --
    #         "REND" block, if any
    #     structs_by_index --
    #         struct type definitions collected from Structure DNA block, indexed by number
    #     thumbnail_block --
    #         "TEST" block, if any
    #     types --
    #         type definitions collected from Structure DNA block, indexed by name
    #     types_by_index
    #         type definitions collected from Structure DNA block, indexed by number
    #     user_prefs_block --
    #         "USER" block, if any
    #     version --
    #         3-character version code from file header
    #     window_manager_block --
    #         "DATA" block of type wmWindowManager

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
    #     "override_type" -- if present, contents were decoded according to this type (dna_index should be 0)
    #     "rawdata" -- the raw, undecoded data (only kept if keep_rawdata is specified to load)
    #     "refs" -- list of references to this block (optional, only present if count_refs is specified to load)
    #     "type" -- (only if the block contents were decodeable) -- the reference to the structure type of the block contents

    def __init__(self, big_endian = None, pointer_size = None, log = None) :
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
        self.log = log
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

    def type_align(self, of_type) :
        if type(of_type) == PointerType :
            result = self.ptrsize
        elif type(of_type) == FixedArrayType :
            result = self.type_align(of_type.EltType)
        elif type(of_type) == MethodType :
            result = self.ptrsize
        elif of_type["name"] in primitive_types :
            result = max(of_type["size"], 1)
        else :
            assert of_type["name"] in self.alignments, "type %s has no alignment" % of_type["name"]
            result = self.alignments[of_type["name"]]
        #end if
        return \
            result
    #end type_align

    def compute_alignment(self, for_type, compute_sizes = False) :
        success = True # to begin with
        if is_struct_type(for_type) and for_type["name"] not in self.alignments :
            struct_size = 0
            struct_align = 1
            for field in for_type["fields"] :
                field_size = self.type_size(field["type"])
                elt_type = field["type"]
                if type(elt_type) == FixedArrayType :
                    elt_type = elt_type.EltType
                #end if
                if is_struct_type(elt_type) and elt_type["name"] not in self.alignments :
                    success = False
                    break
                #end if
                field_align = self.type_align(field["type"])
                struct_align = max(struct_align, field_align)
                struct_size += align_adjust(struct_size, field_align) + field_size
            #end for
            if success :
                if compute_sizes :
                    for_type["size"] = struct_size
                else :
                    assert struct_size == for_type["size"], "size mismatch for struct %s, calc’d %d, got %d" % (for_type["name"], struct_size, for_type["size"])
                #end if
                self.alignments[for_type["name"]] = struct_align
            #end if
        #end if
        return \
            success
    #end compute_alignment

    def compute_alignments(self, compute_sizes = False) :
        # computes alignments for all structs, and if compute_sizes, recomputes
        # their sizes accordingly, else checks that they match what is computed.
        # Checking alignments on load is helpful to ensure that I can generate
        # correct structures on save.
        self.alignments = {}
        while True :
            missed_one = False
            for t in self.types_by_index :
                if not self.compute_alignment(t, compute_sizes = compute_sizes) :
                    missed_one = True
                #end if
            #end for
            if not missed_one :
                break
        #end while
    #end compute_alignments

    def decode_sdna(self, sdna_data) :
        "decodes a structure definitions block and saves the results in instance variables."

        def align_sdna() :
            nonlocal sdna_data, data_offset
            adj = align_adjust(data_offset, 4)
            if adj != 0 :
                sdna_data = sdna_data[adj:]
                data_offset += adj
            #end if
        #end align_sdna

    #begin decode_sdna
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
            nr_names = struct.unpack(self.endian + "I", sdna_data[4:8])[0]
            sdna_data = sdna_data[8:]
            data_offset += 8
            for i in range(nr_names) :
                str_end = sdna_data.index(b"\0")
                collect.append(sdna_data[:str_end].decode("utf-8"))
                if self.log != None :
                    self.log.write("name[%d] = %s\n" % (i, repr(collect[i]))) # debug
                #end if
                data_offset += str_end + 1
                sdna_data = sdna_data[str_end + 1:]
            #end for
            align_sdna()
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
            if self.log != None :
                self.log.write("sizeof(%s) = %d\n" % (self.types_by_index[i]["name"], self.types_by_index[i]["size"])) # debug
            #end if
        #end for
        sdna_data = sdna_data[2 * len(self.types_by_index):]
        data_offset += 2 * len(self.types_by_index)
        align_sdna()
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
                    if self.log != None :
                        self.log.write("%s.%s : %s\n" % (self.types_by_index[struct_type]["name"], field_name, type_name(field_type))) # debug
                    #end if
                #end if
            #end for
            self.types_by_index[struct_type]["fields"] = fields
            if len(self.structs_by_index) < i + 1 :
                self.structs_by_index.extend([None] * (i + 1 - len(self.structs_by_index)))
            #end if
            self.structs_by_index[i] = self.types_by_index[struct_type]
            if self.log != None :
                self.log.write("struct[%d] is %s\n" % (i, self.types_by_index[struct_type]["name"])) # debug
            #end if
            sdna_data = sdna_data[4 * nr_fields:]
        #end for
        self.compute_alignments()
        self.types = {}
        for t in self.types_by_index :
            assert (t["name"] in primitive_types) <= ("fields" not in t), "primitive type %s must not be struct" % t["name"]
            self.types[t["name"]] = t
        #end for
        assert self.structs_by_index[0] == self.types["Link"], "Link type needs to have index 0"
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
            out.write(b"\0" * align_adjust(offset, 4))
        #end for
        out.write(b"TLEN")
        offset = 0
        for thistype in self.types_by_index :
            out.write(struct.pack(self.endian + "H", thistype["size"]))
            offset += 2
        #end for
        out.write(b"\0" * align_adjust(offset, 4))
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

    def decode_data(self, referrer, rawdata, datatype) :
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
                    the_block["refs"].append(referrer)
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
                result.append(self.decode_data(referrer + (i,), rawdata[i * elt_size : (i + 1) * elt_size], datatype.EltType))
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
            field_offset = 0
            for field in datatype["fields"] :
                field_size = self.type_size(field["type"])
                field_align = self.type_align(field["type"])
                if self.log != None :
                    adj = align_adjust(field_offset, field_align)
                    if adj != 0 :
                        self.log.write("decode %s: align %s from %d by %d\n" % (datatype["name"], field["name"], field_offset, adj)) # debug
                    #end if
                #end if
                field_offset += align_adjust(field_offset, field_align)
                if field_offset + field_size > len(rawdata) :
                    if self.log != None :
                        self.log.write("# need at least %d bytes for %s.%s, only got %d\n" % (field_size, datatype["name"], field["name"], len(rawdata) - field_offset))
                    #end if
                    field_offset = len(rawdata) # ignore rest
                    break
                #end if
                result[field["name"]] = self.decode_data(referrer + (field["name"],), rawdata[field_offset : field_offset + field_size], field["type"])
                field_offset += field_size
            #end for
            assert field_offset == len(rawdata), "leftover data after decoding %s struct" % datatype["name"]
        #end if
        return \
            result
    #end decode_data

    default_encode_ref = lambda block : block["oldaddr"]

    def encode_data(self, data, datatype, encode_ref = default_encode_ref) :
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
                    self.encode_data(i, datatype.EltType, encode_ref = encode_ref) for i in data
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
                field_align = self.type_align(field["type"])
                if self.log != None :
                    adj = align_adjust(len(result), field_align)
                    if adj != 0 :
                        self.log.write("encode %s: align %s from %d by %d\n" % (datatype["name"], field["name"], len(result), field_align - len(result) % field_align)) # debug
                    #end if
                #end if
                result += bytes(align_adjust(len(result), field_align))
                assert type(data) == dict or datatype == self.link_type, "encode_data of non-struct value %s, expected type %s" % (repr(data), repr(datatype))
                if type(data) == dict :
                    result += self.encode_data \
                      (
                        (data.__getitem__, data.get)[datatype == self.link_type](field["name"]),
                        field["type"],
                        encode_ref = encode_ref
                      )
                else :
                    raise RuntimeError("don't know how to encode item %s as %s" % (repr(data), repr(datatype)))
                #end if
            #end for
        #end if
        return \
            result
    #end encode_data

    def encode_block(self, block, encode_ref = default_encode_ref) :
        # encodes the specified block contents to raw data and returns it.
        if block["decoded"] :
            block_type = block.get("override_type", self.structs_by_index[block["dna_index"]])
            result = b"".join \
              (
                self.encode_data(item, block_type, encode_ref = encode_ref)
                for item in block["data"]
              )
        else :
            result = block["rawdata"]
        #end if
        return \
            result
    #end encode_block

    def scan_block(self, referrer, referrer_type, referrer_type_name, selector, block, action, encode_ref) :
        # invokes action(referrer, selector, block) on the specified block, followed by
        # all (indirectly or directly) referenced blocks. Traversal stops if action returns
        # False.

        scan_recurse_depth = 0
        max_scan_recurse_depth = 0

        scanned = set() # blocks that have already been scanned

        def scan_block_recurse(referrer, referrer_type, referrer_type_name, selector, block, action) :
            # invokes action on the specified block, followed by all (indirectly or directly)
            # referenced blocks that haven't already been scanned.

            nonlocal scan_recurse_depth, max_scan_recurse_depth

            def check_item(referrer, referrer_type, referrer_type_name, selector, item, itemtype) :

                def check_scan_ref() :
                    # assume at most one level of pointer indirection!
                    if type(item) == BlockRef :
                        scan_block_recurse(referrer, referrer_type, referrer_type_name, selector, item.block, action)
                    #end if
                #end check_scan_ref

                def check_array() :
                    # debug
                    if self.log != None and len(item) != itemtype.NrElts :
                        self.log.write("array %s has %d elts, expected %d\n" % (repr(item), len(item), itemtype.NrElts))
                    #end if
                    #end debug
                    for i in range(itemtype.NrElts) :
                        check_item(item, itemtype, type_name(itemtype), i, item[i], itemtype.EltType)
                    #end for
                #end check_array

                def check_struct() :
                    assert type(item) == dict or itemtype == self.link_type, "non-struct value %s, expected type %s" % (repr(item), repr(itemtype))
                    if type(item) == dict :
                        for field in itemtype["fields"] :
                            fieldval = (item.__getitem__, item.get)[itemtype == self.link_type](field["name"])
                            if fieldval != None :
                                check_item(item, itemtype, type_name(field["type"]), field, fieldval, field["type"])
                            #end if
                        #end for
                    #end if
                #end check_struct

            #begin check_item
                # transitively check other referenced blocks that haven't already
                # been scanned
                if type(itemtype) == PointerType :
                    check_scan_ref()
                elif type(itemtype) == FixedArrayType and itemtype.EltType != self.types["char"] :
                    check_array()
                elif is_struct_type(itemtype) :
                    check_struct()
                #end if
            #end check_item

        #begin scan_block_recurse
            block_ref = encode_ref(block)
            if block_ref not in scanned :
                scan_recurse_depth += 1
                if scan_recurse_depth > max_scan_recurse_depth :
                    max_scan_recurse_depth = scan_recurse_depth
                    if self.log != None :
                        self.log.write("scan block %d at depth %d\n" % (block["index"], scan_recurse_depth)) # debug
                    #end if
                #end if
                scanned.add(block_ref)
                if action(referrer, referrer_type, referrer_type_name, selector, block) :
                    if block["decoded"] :
                        check_item \
                          (
                            referrer,
                            referrer_type,
                            referrer_type_name,
                            selector,
                            block["data"],
                            FixedArrayType
                              (
                                block.get("override_type", self.structs_by_index[block["dna_index"]]),
                                len(block["data"])
                              )
                          )
                    #end if
                #end if
                scan_recurse_depth -= 1
            #end if
        #end scan_block_recurse

    #begin scan_block
        scan_block_recurse(referrer, referrer_type, referrer_type_name, selector, block, action)
    #end scan_block

    def decode_block(self, block, block_type, dna_count = None) :
        block["type"] = block_type
        type_size = self.type_size(block_type)
        decoded = []
        if dna_count == None :
            dna_count = block["dna_count"]
        #end if
        for j in range(dna_count) :
            decoded.append \
              (
                self.decode_data
                  (
                    (block, j),
                    block["rawdata"][j * type_size : (j + 1) * type_size],
                    block_type
                  )
              )
        #end for
        leftover = max(len(block["rawdata"]) - type_size * dna_count, 0)
        if leftover > 0 :
            if self.log != None :
                self.log.write("# ignoring %d bytes at end of block[%d] size %d//%d type %s size %d\n" % (leftover, block["index"], len(block["rawdata"]), dna_count, repr(block_type), type_size))
            #end if
        #end if
        block["data"] = decoded
        block["decoded"] = True
    #end decode_block

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

    def dump_counts(self) :
        # debug--dumps some stats about block types.
        block_codes = {}
        block_types = {}
        block_codes_unrefd = None
        block_types_unrefd = None
        max_name_length = max(len(name) for name in self.types)
        for block in self.blocks :
            if block["decoded"] :
                this_code = block["code"]
                this_type = type_name(block["type"])
                block_codes[this_code] = block_codes.get(this_code, 0) + 1
                block_types[this_type] = block_types.get(this_type, 0) + 1
                if "refs" in block :
                    if block_codes_unrefd == None :
                        block_codes_unrefd = {}
                    #end if
                    if block_types_unrefd == None :
                        block_types_unrefd = {}
                    #end if
                    if len(block["refs"]) == 0 :
                        block_codes_unrefd[this_code] = block_codes_unrefd.get(this_code, 0) + 1
                        block_types_unrefd[this_type] = block_types_unrefd.get(this_type, 0) + 1
                    #end if
                #end if
            #end if
        #end for
        self.log.write("\nBlock code counts:\n")
        for this_code in block_code_order :
            unrefd = block_codes_unrefd.get(this_code, 0)
            self.log.write \
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
                        "count" : block_codes.get(this_code, 0),
                        "unrefd" : unrefd,
                    }
              )
        #end if
        self.log.write("Block type counts:\n")
        for this_type in sorted(self.types) :
            unrefd = block_types_unrefd.get(this_type, 0)
            self.log.write \
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
                        "type" : this_type + "." * (max_name_length - len(this_type)),
                        "count" : block_types.get(this_type, 0),
                        "unrefd" : unrefd,
                    }
              )
        #end for
    #end dump_counts

    def load(self, filename, keep_rawdata = False, count_refs = False) :
        "loads the contents of the specified .blend file."

        encode_ref = self.__class__.default_encode_ref # good enough

        def decode_untyped_blocks() :
          # tries to determine types of undecoded blocks by expected types of
          # pointers to them

            def decode_block_action(referrer, referrer_type, referrer_type_name, selector, block) :
                if not block["decoded"] :
                    assert referrer != None and referrer_type != None and referrer_type_name != None and selector != None, \
                        "undecoded top-level block!?"
                    block_type = None
                    if type(referrer_type) == FixedArrayType :
                        block_type = referrer_type.EltType
                        referrer_type_name = "*" + referrer_type_name
                    elif is_struct_type(referrer_type) :
                        block_type = selector["type"]
                    #end if
                    assert type(block_type) == PointerType, "cannot determine referrer-selector type"
                    block_type = block_type.EltType
                    type_size = self.type_size(block_type)
                    if type_size == 0 : # type is void
                        block_type = make_primitive_type("uchar")
                        type_size = 1
                    #end if
                    dna_count = len(block["rawdata"]) // self.type_size(block_type)
                    if self.log != None :
                        self.log.write("decoding untyped block[%d] as %s[%d]\n" % (block["index"], referrer_type_name, dna_count)) # debug
                    #end if
                    block["override_type"] = block_type
                    self.decode_block(block, block_type, dna_count = dna_count)
                    self.scan_block(referrer, referrer_type, referrer_type_name, selector, block, decode_block_action, encode_ref)
                      # in case it points to further undecoded blocks
                #end if
                return \
                    True
            #end decode_block_action

        #begin decode_untyped_blocks
            self.scan_block(None, None, None, None, self.global_block, decode_block_action, encode_ref)
        #end decode_untyped_blocks

    #begin load
        origfd = open(filename, "rb")
        sig = origfd.read(2)
        origfd.seek(0)
        if sig == b"\x1F\x8B" :
            fd = gzip.GzipFile(mode = "r", fileobj = origfd)
        else :
            fd = origfd
            origfd = None
        #end if
        known_block_codes = frozenset(block_code_order)
        sig, ptrcode, endiancode, self.version = structread(fd, "7s1s1s3s") # note not endian-dependent
        assert sig == blender_sig, "unrecognized file header signature %s" % sig
        self.ptrsize = {b"_" : 4, b"-" : 8}[ptrcode]
        self.ptrcode = {b"_" : "L", b"-" : "Q"}[ptrcode]
        self.endian = {b"v" : "<", b"V" : ">"}[endiancode]
        self.big_endian = endiancode == b"V"
        if self.log != None :
            self.log.write("File Blender version = %s, ptrsize = %d, endian = %s\n" % (repr(self.version), self.ptrsize, ("little", "big")[endiancode == b"V"]))
        #end if
        self.blocks = []
        self.blocks_by_oldaddress = {}
        self.global_block = None
        self.renderinfo_block = None
        self.thumbnail_block = None
        self.user_prefs_block = None
        self.window_manager_block = None
        sdna_seen = False
        while True :
            # collect all the blocks
            blockcode = b"".join(structread(fd, "%s4c" % self.endian))
            if blockcode[:2] == b"\x00\x00" :
                blockcode = blockcode[2:] + blockcode[:2]
            #end if
            assert blockcode[2:] != b"\x00\x00" or blockcode in known_block_codes, \
                "unknown block code %s" % repr(blockcode)
            if blockcode == b"ENDB" :
                # file-end marker block
                # don't try reading rest of block, because it might be truncated in some files
                break
            #end if
            datasize, oldaddr, dna_index, dna_count = \
                structread(fd, "%sI%sII" % (self.endian, self.ptrcode))
            if self.log != None :
                self.log.write("block[%d] at 0x%08x code = %s, datasize = %d, oldaddr = 0x%x, dna_index = %d, dna_count = %d\n" % (len(self.blocks), fd.tell(), blockcode, datasize, oldaddr, dna_index, dna_count)) # debug
            #end if
            if blockcode == b"DNA1" :
                assert not sdna_seen, "duplicate SDNA blocks"
                self.decode_sdna(fd.read(datasize))
                sdna_seen = True
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
                    new_block["refs"] = []
                #end if
                if blockcode == b"GLOB" :
                    assert self.global_block == None, "multiple GLOB blocks found"
                    self.global_block = new_block
                elif blockcode == b"REND" :
                    self.renderinfo_block = new_block
                elif blockcode == b"TEST" :
                    self.thumbnail_block = new_block
                elif blockcode == b"USER" :
                    self.user_prefs_block = new_block
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
        for block in self.blocks :
            block_type = self.structs_by_index[block["dna_index"]]
            if block["code"] == b"REND" :
                block["type"] = \
                    {
                        "name" : " render_block_type", # try to guarantee name won't clash
                        "fields" :
                            [
                                {"name" : "sfra", "type" : make_primitive_type("int")},
                                {"name" : "efra", "type" : make_primitive_type("int")},
                                {
                                    "name" : "scene_name",
                                    "type" :
                                        FixedArrayType
                                          (
                                            make_primitive_type("char"),
                                            max(0, min(len(block["rawdata"]) - 2 * primitive_types["int"]["size"], MAX_ID_NAME - 2)),
                                          ),
                                },
                            ],
                    }
                self.compute_alignment(block["type"], compute_sizes = True)
                block["data"] = self.decode_data((), block["rawdata"], block["type"])
                block["decoded"] = True
            elif block["code"] == b"TEST" :
                width, height = struct.unpack(self.endian + "ii", block["rawdata"][:8])
                block["type"] = \
                    {
                        "name" : " thumbnail_block_type", # try to guarantee name won't clash
                        "fields" :
                            [
                                {"name" : "width", "type" : make_primitive_type("int")},
                                {"name" : "height", "type" : make_primitive_type("int")},
                                {
                                    "name" : "pixels",
                                    "type" :
                                        FixedArrayType(make_primitive_type("uchar"), width * height * 4),
                                },
                            ],
                    }
                self.compute_alignment(block["type"], compute_sizes = True)
                block["data"] = self.decode_data((), block["rawdata"], block["type"])
                block["decoded"] = True
            elif block_type != self.link_type :
              # blocks of Link type are left to decode_untyped_blocks,
              # because they do not seem to actually be of Link type.
                self.decode_block(block, block_type)
                if type_name(block_type) == "wmWindowManager" :
                    assert self.window_manager_block == None, "more than one wmWindowManager block found"
                    self.window_manager_block = block
                #end if
            else :
                block["decoded"] = False
            #end if
            if block["decoded"] and not keep_rawdata :
                del block["rawdata"]
            #end if
        #end for
        decode_untyped_blocks()
        if self.log != None :
            self.dump_counts() # debug
        #end if
        return \
            self
    #end load

    def save \
      (
        self,
        filename,
        compressed = False,
        bit64 = None,
          # can be False to force 32-bit pointers, True to force 64-bit, None to leave at default
        big_endian = None,
          # can be True or False to force big/little-endian, None to leave at default
        encode_ref = default_encode_ref,
        use_rawdata = False,
      ) :
        "saves the contents into a .blend file."

        referenced = set() # set of blocks that should be written out
        saved = set() # set of blocks that have been written out, to avoid double-saving

        def find_referenced() :
            # marks all specially-coded blocks needing saving.

            def referenced_action(referrer, referrer_type, referrer_type_name, selector, block) :
                if block["code"] != b"DATA" :
                    referenced.add(encode_ref(block))
                #end if
                return \
                    True
            #end referenced_action

        #begin find_referenced
            self.scan_block(None, None, None, None, self.global_block, referenced_action, encode_ref)
            for extra_block in (self.window_manager_block, self.user_prefs_block) :
              # extra blocks that need to be saved, even if there are no explicit references to them from anywhere else
                if extra_block != None :
                    self.scan_block(None, None, None, None, extra_block, referenced_action, encode_ref)
                #end if
            #end for
        #end find_referenced

        def save_block(block) :
            # saves the specified block, followed by all (indirectly or directly)
            # referenced DATA blocks that haven't already been saved.

            done_coded = False

            def save_action(referrer, referrer_type, referre_type_name, selector, block) :
                nonlocal done_coded
                block_ref = encode_ref(block)
                doit = block_ref not in saved and (block["code"] == b"DATA" or not done_coded)
                  # stop at next non-DATA block
                if doit :
                    saved.add(block_ref)
                    if block["code"] != b"DATA" :
                        done_coded = True
                    #end if
                    outfile.write \
                      (
                        self.construct_block
                          (
                            block["code"],
                            block_ref,
                            block["dna_index"],
                            block["dna_count"],
                            (
                                lambda : self.encode_block(block, encode_ref = encode_ref),
                                lambda : block["rawdata"],
                            )[not block["decoded"] or use_rawdata and "rawdata" in block]()
                          )
                      )
                #end if
                return \
                    doit
            #end save_action

        #begin save_block
            self.scan_block(None, None, None, None, block, save_action, encode_ref)
        #end save_block

        def save_special_block(block) :
            outfile.write \
              (
                self.construct_block
                  (
                    block["code"],
                    encode_ref(block),
                    0, # block["dna_index"],
                    1, # block["dna_count"],
                    (
                        lambda : self.encode_data(block["data"], block["type"], encode_ref = encode_ref),
                        lambda : block["rawdata"],
                    )[not block["decoded"] or use_rawdata and "rawdata" in block]()
                  )
              )
        #end save_special_block

    #begin save
        if bit64 != None :
            self.ptrsize = [4, 8][bit64]
            self.ptrcode = {4 : "L", 8 : "Q"}[self.ptrsize]
            self.compute_alignments(compute_sizes = True)
              # Note this does not reprocess the synthesized types for REND and TEST blocks,
              # since these are special-cased on creation and not included in types_by_index.
              # But that's OK, since they don't contain any pointers or fields requiring alignment.
        #end if
        if big_endian != None :
            self.big_endian = big_endian
            self.endian = {False : "<", True : ">"}[big_endian]
        #end if
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
        if self.renderinfo_block != None :
            save_special_block(self.renderinfo_block)
        #end if
        if self.thumbnail_block != None :
            save_special_block(self.thumbnail_block)
        #end if
        save_block(self.global_block)
        for code in block_code_order :
            for block in self.blocks :
                if block["code"] == code and encode_ref(block) in referenced :
                    save_block(block)
                #end if
            #end for
        #end for
        if self.user_prefs_block != None :
            save_block(self.user_prefs_block)
        #end if
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
        return \
            self
    #end save

#end Blenddata
