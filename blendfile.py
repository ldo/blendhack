#+
# Parsing of .blend files created by Blender <http://www.blender.org/>.
#
# For further info, see
#     "The Mystery of the Blend" <http://www.atmind.nl/blender/mystery_ot_blend.html>
#     The Blender source code, doc/blender_file_format subdirectory
#
# Copyright 2012 Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
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
    # for showing pointers that don't seem to point to any valid block

    def __init__(self, addr) :
        self.addr = addr
    #end __init__

    def __repr__(self) :
        return "DanglingPointer(0x%x)" % self.addr
    #end __repr__

#end DanglingPointer

class RoutineAddr :
    # for showing pointers to methods

    def __init__(self, addr) :
        self.addr = addr
    #end __init__

    def __repr__(self) :
        return "RoutineAddr(0x%x)" % self.addr
    #end __repr__

#end RoutineAddr

class BlockIndex :
    # for showing references to other blocks

    def __init__(self, index) :
        self.index = index
    #end __init__

    def __repr__(self) :
        return "*Block[%d]" % self.index
    #end __repr__

#end BlockIndex

#+
# Handling of Blender types
#-

class PointerType :
    # represents a pointer

    def __init__(self, EltType) :
        self.EltType = EltType
    #end __init__

#end PointerType

class FixedArrayType :
    # represents a fixed-length array

    def __init__(self, EltType, NrElts) :
        self.EltType = EltType
        self.NrElts = NrElts
    #end __init__

#end FixedArrayType :

class MethodType :
    # represents a pointer to a routine

    def __init__(self, ResultType) :
        self.ResultType = ResultType
    #end __init__

#end MethodType

def parse_field_type(field_name, field_type) :
    # processes special forms of field_name ("*name", "name[ind]", "(*name)()"),
    # returning inner name and adjusting field_type accordingly.
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
    {
        "char" : {"code" : "c", "size" : 1},
        "uchar" : {"code" : "B", "size" : 1},
        "short" : {"code" : "h", "size" : 2},
        "ushort" : {"code" : "H", "size" : 2},
        "int" : {"code" : "i", "size" : 4},
        "long" : {"code" : "l", "size" : 4},
        "ulong" : {"code" : "L", "size" : 4},
        "float" : {"code" : "f", "size" : 4},
        "double" : {"code" : "d", "size" : 8},
        "int64_t" : {"code" : "q", "size" : 8}, # might not be present
        "uint64_t" : {"code" : "Q", "size" : 8}, # might not be present
        "void" : {"size" : 0}, # only occurs as pointer object type!
    }

class Blenddata :
    "Instantiate this to parse a .blend file, parsing it the pathname of the" \
    " file to read and parse. The returned object will contain the parsed" \
    " contents of the file."

    # Instance variables:
    #     blocks --
    #         list of blocks from file, in order of appearance
    #     blocks_by_old_address --
    #         table of blocks from file, indexed by old-address field
    #     endian --
    #         prefix code to use with struct.unpack to indicate endianness of data
    #     global_block --
    #         "GLOB" block
    #     ptrcode --
    #         code to use to struct.unpack to decode a pointer field
    #     ptrsize --
    #         size in bytes of an address according to version of Blender which created the file
    #     types --
    #         type definitions collected from Structure DNA block, indexed by name
    #     version --
    #         3-character version code from file header

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

    def decode_data(self, rawdata, datatype, log) :
        # decodes the bytes of rawdata to Python form according to the type datatype.
        if type(datatype) == PointerType :
            assert len(rawdata) == self.ptrsize, "expecting pointer to be %d bytes, got %d" % (self.ptrsize, len(rawdata))
            oldaddress = struct.unpack(self.endian + self.ptrcode, rawdata)[0]
            if oldaddress == 0 :
                result = None
            elif oldaddress in self.blocks_by_oldaddress :
                # result = self.blocks_by_oldaddress[oldaddress]
                result = BlockIndex(self.blocks_by_oldaddress[oldaddress]["index"]) # less cluttered display
            else :
                result = DanglingPointer(oldaddress)
            #end if
        elif type(datatype) == FixedArrayType :
            result = []
            elt_size = self.type_size(datatype.EltType)
            assert len(rawdata) == elt_size * datatype.NrElts
            for i in range(0, datatype.NrElts) :
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

    def __init__(self, filename, keep_rawdata = False, log = None) :
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
        sig, ptrcode, endiancode, self.version = structread(fd, "7s1s1s3s")
        assert sig == b"BLENDER", "unrecognized file header signature %s" % sig
        self.ptrsize = {b"_" : 4, b"-" : 8}[ptrcode]
        self.ptrcode = {b"_" : "L", b"-" : "Q"}[ptrcode]
        self.endian = {b"v" : "<", b"V" : ">"}[endiancode]
        log.write("File Blender version = %s, ptrsize = %d, endian = %s\n" % (repr(self.version), self.ptrsize, ("little", "big")[endiancode == "V"]))
        self.blocks = []
        self.blocks_by_oldaddress = {}
        self.global_block = None
        while True :
            # collect all the blocks
            blockcode, datasize, oldaddr, dna_index, dna_count = \
                structread(fd, "%s4sI%sII" % (self.endian, self.ptrcode))
            log.write("blockcode = %s, datasize = %d, oldattr = 0x%x, dna_index = %d, dna_count = %d\n" % (blockcode, datasize, oldaddr, dna_index, dna_count)) # debug
            if blockcode == b"DNA1" :
                # decode the structure definitions
                sdna_data = fd.read(datasize)
                sdna_id = sdna_data[:4]
                assert sdna_id == b"SDNA", "invalid DNA block header"
                sdna_data = sdna_data[4:]
                names = []
                types_by_index = []
                structs = []
                data_offset = 4
                for \
                    expect_id, collect \
                in \
                    (
                        (b"NAME", names),
                        (b"TYPE", types_by_index),
                    ) \
                :
                    assert len(sdna_data) >= 8, "premature end of DNA block"
                    assert expect_id == sdna_data[:4], "expecting %s sub-block in DNA block" % expect_id
                    nr_names = struct.unpack(self.endian + "I", sdna_data[4:8])[0]
                    sdna_data = sdna_data[8:]
                    data_offset += 8
                    for i in range(0, nr_names) :
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
                for i in range(0, len(types_by_index)) :
                    types_by_index[i] = {"name" : types_by_index[i]}
                #end for
                assert sdna_data[:4] == b"TLEN", "expecting TLEN sub-block in DNA block"
                sdna_data = sdna_data[4:]
                data_offset += 4
                for i, s in enumerate(struct.unpack(self.endian + "H" * len(types_by_index), sdna_data[:2 * len(types_by_index)])) :
                    types_by_index[i]["size"] = s
                    log.write("sizeof(%s) = %d\n" % (types_by_index[i]["name"], types_by_index[i]["size"])) # debug
                #end for
                sdna_data = sdna_data[2 * len(types_by_index):]
                data_offset += 2 * len(types_by_index)
                if data_offset % 4 != 0 :
                    sdna_data = sdna_data[4 - data_offset % 4:]
                    data_offset += 4 - data_offset % 4
                #end if
                assert sdna_data[:4] == b"STRC", "expecting STRC sub-block in DNA block"
                nr_structs = struct.unpack(self.endian + "I", sdna_data[4:8])[0]
                sdna_data = sdna_data[8:]
                data_offset += 8
                for i in range(0, nr_structs) :
                    struct_type, nr_fields = struct.unpack(self.endian + "HH", sdna_data[:4])
                    sdna_data = sdna_data[4:]
                    fields = []
                    for j, f in enumerate(struct.unpack(self.endian + "HH" * nr_fields, sdna_data[:4 * nr_fields])) :
                        if j % 2 == 0 :
                            field_type = f
                        else :
                            field_name, field_type = parse_field_type(names[f], types_by_index[field_type])
                            fields.append({"type" : field_type, "name" : field_name})
                            log.write("%s.%s : %s\n" % (types_by_index[struct_type]["name"], field_name, type_name(field_type))) # debug
                        #end if
                    #end for
                    types_by_index[struct_type]["fields"] = fields
                    if len(structs) < i + 1 :
                        structs.extend([None] * (i + 1 - len(structs)))
                    #end if
                    structs[i] = types_by_index[struct_type]
                    log.write("struct[%d] is %s\n" % (i, types_by_index[struct_type]["name"])) # debug
                    sdna_data = sdna_data[4 * nr_fields:]
                #end for
                self.types = {}
                for t in types_by_index :
                    assert (t["name"] in primitive_types) <= ("fields" not in t), "primitive type %s must not be struct" % t["name"]
                    self.types[t["name"]] = t
                #end for
                for k in primitive_types :
                    assert k not in self.types or primitive_types[k]["size"] == self.types[k]["size"], "wrong type size for primitive type %s, expected %d, got %d" % (k, primitive_types[k]["size"], self.types[k]["size"])
                #end for
            elif blockcode == b"ENDB" :
                # file-end marker block
                break
            else :
                new_block = \
                    {
                        "code" : blockcode,
                        "oldaddr" : oldaddr,
                        "dna_index" : dna_index,
                        "dna_count" : dna_count,
                        "rawdata" : fd.read(datasize),
                        "index" : len(self.blocks),
                    }
                if blockcode == b"GLOB" :
                    assert self.global_block == None, "multiple GLOB blocks found"
                    self.global_block = new_block
                #end if
                self.blocks.append(new_block)
                self.blocks_by_oldaddress[new_block["oldaddr"]] = new_block
            #end if
        #end while
        fd.close()
        if origfd != None :
            origfd.close()
        #end if
        assert self.global_block != None, "missing GLOB block"
        for block in self.blocks :
            block_type = structs[block["dna_index"]]
            block["type"] = block_type
            decoded = []
            for j in range(0, block["dna_count"]) :
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
            block["data"] = decoded
            if not keep_rawdata :
                del block["rawdata"]
            #end if
        #end for
        if openlog != None :
            openlog.close()
        #end if
    #end __init__

#end Blenddata
