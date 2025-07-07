import gleam/bit_array
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string

/// Represents all possible values in the MessagePack serialization format.
///
/// This type is used as an intermediate representation for converting
/// between user-defined types and serialized binary data.
///
/// ## Variants
/// - `Nil`: Represents a null or absent value.
/// - `Bool`: Represents a `true` or `false` boolean value.
/// - `Int`: Represents a signed or unsigned integer.
/// - `Float`: Represents a 64-bit floating-point value.
/// - `String`: Represents a UTF-8 encoded text string.
/// - `Binary`: Represents raw binary data using a flat `BitArray`.
/// - `BinaryTree`: Represents binary data using a structured `BytesTree`,
///    which is more efficient for streaming and incremental construction.
/// - `Array`: Represents a list of MsgPack values.
/// - `Map`: Represents key-value pairs of MsgPack values.
/// - `Extended`: Represents an extension type with a type tag and payload.
///
/// ## Notes
/// - `BinaryTree` is preferred when constructing messages incrementally.
/// - `Extended` corresponds to MsgPack’s "ext" type, which is
///    used to encode application-defined data formats.
/// - Keys in `Map` can be any MsgPack value, not just strings.
pub type MsgPack {
  /// Represents a null or absent value.
  MsgNil

  /// Boolean value: true or false.
  MsgBool(Bool)

  /// Integer value (signed or unsigned).
  MsgInt(Int)

  /// 64-bit floating-point value.
  MsgFloat(Float)

  /// UTF-8 string value.
  MsgString(String)

  /// Raw binary data as a flat `BitArray`.
  MsgBinary(BitArray)

  /// Ordered list of MsgPack values.
  MsgArray(List(MsgPack))

  /// Map of MsgPack keys to MsgPack values.
  MsgMap(List(#(MsgPack, MsgPack)))
}

pub type DecodeError {
  // low-level error message
  UnableToParse(String)
  // converter failure
  UnableToDecode(String)
  // unsupported type
  UnsupportedDecode
  // input shorter than a header
  Truncated
}

pub type EncodeError {
  UnsupportedEncode(String)
  Overflow(String)
  Internal(String)
}

fn decode_utf8_string(
  bits: BitArray,
  start: Int,
  byte_len: Int,
) -> Result(String, DecodeError) {
  let skip = start / 8

  case bits {
    <<_skip:bytes-size(skip), str_bytes:bytes-size(byte_len), _rest:bits>> ->
      bit_array.to_string(str_bytes)
      |> result.map_error(fn(_) {
        UnableToDecode("Unable to read string from data bits")
      })

    _ -> Error(UnableToDecode("Invalid or missing str8 length"))
  }
}

// Parses "<<1, 2, 3>>" → [1, 2, 3]
fn parse_inspected_bytes(text: String) -> List(Int) {
  let trimmed = string.trim(text)

  let dropped_start = case string.starts_with(trimmed, "<<") {
    True -> string.drop_start(from: trimmed, up_to: 2)
    False -> trimmed
  }

  let dropped_end = case string.ends_with(dropped_start, ">>") {
    True -> string.drop_end(from: dropped_start, up_to: 2)
    False -> dropped_start
  }

  string.split(dropped_end, on: ",")
  |> list.map(string.trim)
  |> list.filter_map(int.parse)
}

fn bit_array_to_int_unsigned(bits: BitArray) -> Int {
  bits
  |> bit_array.inspect
  |> parse_inspected_bytes
  |> list.fold(0, fn(acc, byte) { int.add(int.multiply(acc, 256), byte) })
}

/// Return the number of **bits** occupied by the MsgPack value that starts
/// at `tag`.  For composite types (array, map, ext) this returns header
/// length only; the caller continues decoding element-by-element.
fn value_bit_len(tag: Int, _bits: BitArray) -> Int {
  case tag {
    // ── Fix-int ranges ────────────────────────────────────────────────
    t if t <= 0x7F -> 8
    // positive fixint
    t if t >= 0xE0 -> 8

    // negative fixint (signed 5-bit)
    // ── Fix-string (A0‥BF)  tag byte encodes length in lower 5 bits ──
    t if t >= 0xA0 && t <= 0xBF -> {
      let payload_bytes = t - 0xA0
      let total_bytes = payload_bytes + 1
      // include tag byte
      total_bytes * 8
    }

    // ── Primitive one-shot lengths ────────────────────────────────────
    0xC0 -> 8
    // nil
    0xC2 -> 8
    // false
    0xC3 -> 8

    // true
    0xCA -> 40
    // f32  (1 + 4 bytes) * 8
    0xCB -> 72

    // f64  (1 + 8 bytes) * 8
    0xCC -> 16
    // uint8
    0xCD -> 24
    // uint16
    0xCE -> 40
    // uint32
    0xCF -> 72

    // uint64
    0xD0 -> 16
    // int8
    0xD1 -> 24
    // int16
    0xD2 -> 40
    // int32
    0xD3 -> 72

    // int64
    // ── bin8 / bin16 / bin32 header sizes (payload length handled by caller)
    0xC4 -> 16
    // tag + 1-byte length  =  2 bytes  = 16 bits
    0xC5 -> 24
    // tag + 2-byte length  =  3 bytes  = 24 bits
    0xC6 -> 40

    // tag + 4-byte length  =  5 bytes  = 40 bits
    // ── array16 / array32 header lengths  (element loop adds payload bits)
    0xDC -> 24
    // tag + 2-byte count
    0xDD -> 40

    // tag + 4-byte count
    // ── map16 / map32 header lengths
    0xDE -> 24
    0xDF -> 40

    // Fallback: unknown tag – assume just the tag byte so decoding can fail later
    _ -> 8
  }
}

// Encode Section //

fn pack_nil() -> MsgPack {
  MsgNil
}

fn pack_bool(value: Bool) -> MsgPack {
  MsgBool(value)
}

fn pack_int(i: Int) -> MsgPack {
  MsgInt(i)
}

fn pack_string(s: String) -> MsgPack {
  MsgString(s)
}

fn pack_float(f: Float) -> MsgPack {
  MsgFloat(f)
}

fn pack_binary(data: BitArray) -> MsgPack {
  MsgBinary(data)
}

fn array(from elements: List(a), of inner_type: fn(a) -> MsgPack) -> MsgPack {
  let encoded_elements = elements |> list.map(inner_type)
  MsgArray(encoded_elements)
}

fn map(
  from pairs: List(#(k, v)),
  of key_type: fn(k) -> MsgPack,
  and value_type: fn(v) -> MsgPack,
) -> MsgPack {
  let encoded_pairs =
    pairs
    |> list.map(fn(pair) {
      let #(k, v) = pair
      #(key_type(k), value_type(v))
    })

  MsgMap(encoded_pairs)
}

fn to_bit_array(mp: MsgPack) -> Result(BitArray, EncodeError) {
  encode(mp)
}

pub fn encode(mp: MsgPack) -> Result(BitArray, EncodeError) {
  case mp {
    MsgNil -> Ok(<<0xC0:8>>)

    MsgBool(True) -> Ok(<<0xC3:8>>)
    MsgBool(False) -> Ok(<<0xC2:8>>)

    MsgInt(i) -> Ok(encode_int_bits(i))

    MsgFloat(f) -> Ok(encode_float_bits(f))

    MsgString(s) -> Ok(encode_string_bits(s))

    MsgBinary(bytes) -> Ok(encode_bin_bits(bytes))

    MsgArray(xs) ->
      encode_list(xs)
      |> result.map(encode_array_bits)

    MsgMap(pairs) ->
      encode_pairs(pairs)
      |> result.map(encode_map_bits)
  }
}

fn encode_int_bits(i: Int) -> BitArray {
  case i {
    x if x >= 0 && x <= 0x7F -> <<x:8>>

    x if x >= -32 && x < 0 -> {
      let u = x + 256
      // wrap to unsigned byte
      <<u:8>>
    }

    x if x >= -128 && x <= 127 -> <<0xD0:8, x:8>>

    x if x >= -32_768 && x <= 32_767 -> <<0xD1:8, x:big-size(16)>>

    x if x >= -2_147_483_648 && x <= 2_147_483_647 -> <<0xD2:8, x:big-size(32)>>

    _ -> <<0xD3:8, i:big-size(64)>>
  }
}

fn encode_float_bits(f: Float) -> BitArray {
  // 0xCB tag  + IEEE-754 64-bit big-endian payload
  <<0xCB:8, f:float>>
}

fn encode_string_bits(s: String) -> BitArray {
  let utf8 = bit_array.from_string(s)
  let len = bit_array.byte_size(utf8)

  case len {
    l if l <= 31 -> {
      let tag = 0xA0 + l
      <<tag:8, utf8:bits>>
    }

    l if l <= 255 -> <<0xD9:8, l:8, utf8:bits>>

    l if l <= 65_535 -> <<0xDA:8, l:big-size(16), utf8:bits>>

    l -> <<0xDB:8, l:big-size(32), utf8:bits>>
  }
}

fn encode_bin_bits(bytes: BitArray) -> BitArray {
  let len = bit_array.byte_size(bytes)

  case len {
    l if l <= 255 -> <<0xC4:8, l:8, bytes:bits>>

    l if l <= 65_535 -> <<0xC5:8, l:big-size(16), bytes:bits>>

    l -> <<0xC6:8, l:big-size(32), bytes:bits>>
  }
}

fn encode_list(xs: List(MsgPack)) -> Result(List(BitArray), EncodeError) {
  list.fold(xs, Ok([]), encode_list_folder)
  |> result.map(list.reverse)
}

fn encode_list_folder(
  acc_result: Result(List(BitArray), EncodeError),
  item: MsgPack,
) -> Result(List(BitArray), EncodeError) {
  case acc_result {
    Error(e) -> Error(e)
    Ok(acc) ->
      case encode(item) {
        Ok(bits) -> Ok([bits, ..acc])
        Error(e) -> Error(e)
      }
  }
}

fn encode_pairs(
  pairs: List(#(MsgPack, MsgPack)),
) -> Result(List(BitArray), EncodeError) {
  list.fold(pairs, Ok([]), encode_pairs_folder)
  |> result.map(list.reverse)
}

fn encode_pairs_folder(
  acc_result: Result(List(BitArray), EncodeError),
  pair: #(MsgPack, MsgPack),
) -> Result(List(BitArray), EncodeError) {
  case acc_result {
    Error(e) -> Error(e)
    Ok(acc) -> {
      let #(k, v) = pair

      case encode(k) {
        Error(e) -> Error(e)
        Ok(k_bits) ->
          case encode(v) {
            Error(e) -> Error(e)
            Ok(v_bits) -> Ok([v_bits, k_bits, ..acc])
          }
      }
    }
  }
}

fn encode_array_bits(children: List(BitArray)) -> BitArray {
  let len = list.length(children)
  let prefix = case len {
    l if l <= 15 -> {
      let tag = 0x90 + l
      <<tag:8>>
    }

    l if l <= 65_535 -> <<0xDC:8, l:big-size(16)>>

    l -> <<0xDD:8, l:big-size(32)>>
  }

  bit_array.concat([prefix, bit_array.concat(children)])
}

fn encode_map_bits(flat_children: List(BitArray)) -> BitArray {
  // Two elements (key + value) for every map entry
  let pair_count = case int.floor_divide(list.length(flat_children), 2) {
    Ok(n) -> n
    Error(_) -> 0
    // divisor is 2, so this never happens
  }

  let prefix = case pair_count {
    n if n <= 15 -> {
      let tag = 0x80 + n
      <<tag:8>>
    }

    n if n <= 65_535 -> <<0xDE:8, n:big-size(16)>>

    n -> <<0xDF:8, n:big-size(32)>>
  }

  bit_array.concat([prefix, bit_array.concat(flat_children)])
}

// Decode Section //

fn decode_uint8(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:unsigned-size(8), _rest:bits>> -> {
      Ok(MsgInt(value))
    }

    _ -> Error(UnableToDecode("Unable to decode uint8 from BitArray"))
  }
}

fn decode_uint16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:unsigned-size(16), _rest:bits>> -> {
      Ok(MsgInt(value))
    }
    _ -> Error(UnableToDecode("Invalid or missing uint16 format"))
  }
}

fn decode_uint32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:unsigned-size(32), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing uint32 format"))
  }
}

fn decode_uint64(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:unsigned-size(64), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing uint64 format"))
  }
}

fn decode_int8(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:signed-size(8), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing int8 format"))
  }
}

fn decode_int16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:signed-size(16), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing int16 format"))
  }
}

fn decode_int32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:signed-size(32), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing int32 format"))
  }
}

fn decode_int64(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:signed-size(64), _rest:bits>> ->
      Ok(MsgInt(value))
    _ -> Error(UnableToDecode("Invalid or missing int64 format"))
  }
}

fn decode_f32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:float-size(32), _rest:bits>> ->
      Ok(MsgFloat(value))
    _ -> Error(UnableToDecode("Invalid or missing f32 format"))
  }
}

fn decode_f64(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), value:float-size(64), _rest:bits>> ->
      Ok(MsgFloat(value))

    _ -> Error(UnableToDecode("Invalid or missing f64 value"))
  }
}

fn decode_fixstr(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<tag:unsigned-size(8), _rest:bits>> if tag >= 0xA0 && tag <= 0xBF -> {
      let len = tag - 0xA0
      decode_utf8_string(bits, 8, len)
      |> result.map(MsgString)
    }

    _ -> Error(UnableToDecode("Invalid or missing fixstr tag"))
  }
}

fn decode_str8(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), len:unsigned-size(8), _rest:bits>> ->
      decode_utf8_string(bits, 16, len)
      |> result.map(MsgString)

    _ -> Error(UnableToDecode("Invalid or missing str8 length"))
  }
}

fn decode_str16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), len:unsigned-size(16)-big, _rest:bits>> ->
      decode_utf8_string(bits, 24, len)
      |> result.map(MsgString)

    _ -> Error(UnableToDecode("Invalid or too short str16 BitArray"))
  }
}

fn decode_str32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), len:unsigned-size(32), _rest:bits>> ->
      decode_utf8_string(bits, 40, len)
      |> result.map(MsgString)

    _ -> Error(UnableToDecode("Invalid or too short str32 BitArray"))
  }
}

fn decode_strn_with_len(
  bits: BitArray,
  len_bytes: Int,
) -> Result(#(MsgPack, Int), DecodeError) {
  let len_bits = len_bytes * 8
  let tag_bits = 8

  case bits {
    <<_tag:unsigned-size(tag_bits), len:unsigned-size(len_bits), rest:bits>> -> {
      let payload_bits = len * 8
      let total_bits = tag_bits + len_bits + payload_bits

      case rest {
        <<payload:bits-size(payload_bits), _rest:bits>> ->
          case decode_utf8_string(payload, 0, len) {
            Ok(s) -> Ok(#(MsgString(s), total_bits))
            Error(e) -> Error(e)
          }

        _ -> Error(UnableToDecode("Unable to slice string payload"))
      }
    }

    _ -> Error(UnableToDecode("Unable to parse string length header"))
  }
}

fn decode_bin8(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<
      _tag:unsigned-size(8),
      len:unsigned-size(8),
      payload:bytes-size(len),
      _rest:bits,
    >> -> Ok(MsgBinary(payload))

    _ -> Error(UnableToDecode("Invalid or incomplete bin8"))
  }
}

fn decode_bin16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), len:unsigned-size(16), rest:bits>> -> {
      let total_bits = len * 8
      case rest {
        <<payload:bits-size(total_bits), _remainder:bits>> -> {
          Ok(MsgBinary(payload))
        }
        _ -> Error(UnableToDecode("Failed to extract bin16 payload"))
      }
    }
    _ -> Error(UnableToDecode("Invalid or missing bin16 length"))
  }
}

fn decode_bin32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), len:unsigned-size(32), rest:bits>> -> {
      let total_bits = len * 8
      case rest {
        <<payload:bits-size(total_bits), _remainder:bits>> -> {
          Ok(MsgBinary(payload))
        }
        _ -> Error(UnableToDecode("Failed to extract bin32 payload"))
      }
    }
    _ -> Error(UnableToDecode("Invalid or missing bin32 length"))
  }
}

fn decode_bin_with_len(
  bits: BitArray,
  size_bits: Int,
) -> Result(#(MsgPack, Int), DecodeError) {
  case bits {
    <<
      _tag:unsigned-size(8),
      len:unsigned-size(8),
      content:bytes-size(len),
      _rest:bits,
    >>
      if size_bits == 8
    -> Ok(#(MsgBinary(content), 8 + 8 + len * 8))

    <<
      _tag:unsigned-size(8),
      len:unsigned-size(16),
      content:bytes-size(len),
      _rest:bits,
    >>
      if size_bits == 16
    -> Ok(#(MsgBinary(content), 8 + 16 + len * 8))

    <<
      _tag:unsigned-size(8),
      len:unsigned-size(32),
      content:bytes-size(len),
      _rest:bits,
    >>
      if size_bits == 32
    -> Ok(#(MsgBinary(content), 8 + 32 + len * 8))

    _ -> Error(UnableToDecode("Unable to decode binary with given length tag"))
  }
}

fn decode_fixarray(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<tag:unsigned-size(8), _rest:bits>> if tag >= 0x90 && tag <= 0x9F -> {
      let count = tag - 0x90
      decode_array_items(bits, 8, count)
      |> result.map(fn(array) { array })
    }

    _ -> Error(UnableToDecode("Invalid or missing fixarray tag"))
  }
}

fn decode_array16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    // 0xDC tag (ignored), 16-bit big-endian element count
    <<_tag:unsigned-size(8), count:unsigned-size(16), _rest:bits>> ->
      // The payload starts 24 bits in (1 byte tag + 2 bytes length)
      decode_array_items(bits, 24, count)

    _ -> Error(UnableToDecode("Invalid or too short array16 BitArray"))
  }
}

fn decode_array32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    // 0xDD tag (ignored), 32-bit big-endian element count
    <<_tag:unsigned-size(8), count:unsigned-size(32), _rest:bits>> ->
      // Payload begins after 8 + 32 = 40 bits
      decode_array_items(bits, 40, count)

    _ -> Error(UnableToDecode("Invalid or too short array32 BitArray"))
  }
}

fn decode_array_items(
  bits: BitArray,
  start: Int,
  count: Int,
) -> Result(MsgPack, DecodeError) {
  decode_array_loop(bits, start, count, [])
}

fn decode_array_loop(
  bits: BitArray,
  at: Int,
  remain: Int,
  acc: List(MsgPack),
) -> Result(MsgPack, DecodeError) {
  case remain {
    0 -> Ok(MsgArray(list.reverse(acc)))

    _ -> {
      case decode_msgpack_from_bit_offset(bits, at) {
        Ok(#(elem, next_bit)) ->
          decode_array_loop(bits, next_bit, remain - 1, [elem, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

fn decode_value_with_offset(
  tag: Int,
  bits: BitArray,
  offset: Int,
) -> Result(#(MsgPack, Int), DecodeError) {
  // Call your current single-value decoder (returns MsgPack)
  case decode_value(tag, bits) {
    Ok(msg) -> {
      // `value_bit_len(tag, bits)` must compute how many bits were
      // consumed for *this* tag.  You already know that length logic
      // in each per-type decoder; surface it here in a helper.
      let consumed = value_bit_len(tag, bits)
      Ok(#(msg, offset + consumed))
    }
    Error(e) -> Error(e)
  }
}

fn decode_value(tag: Int, bytes: BitArray) -> Result(MsgPack, DecodeError) {
  case tag {
    // NIL
    0xC0 -> Ok(MsgNil)

    // BOOL
    0xC2 -> Ok(MsgBool(False))
    0xC3 -> Ok(MsgBool(True))

    // POSITIVE FIXINT
    t if t >= 0x00 && t <= 0x7F -> Ok(MsgInt(t))

    // NEGATIVE FIXINT
    t if t >= 0xE0 && t <= 0xFF -> Ok(MsgInt(int.subtract(t, 256)))

    // UINT
    0xCC -> decode_uint8(bytes)
    0xCD -> decode_uint16(bytes)
    0xCE -> decode_uint32(bytes)
    0xCF -> decode_uint64(bytes)

    // INT
    0xD0 -> decode_int8(bytes)
    0xD1 -> decode_int16(bytes)
    0xD2 -> decode_int32(bytes)
    0xD3 -> decode_int64(bytes)

    // FLOAT
    0xCA -> decode_f32(bytes)
    0xCB -> decode_f64(bytes)

    // STRING
    t if t >= 0xA0 && t <= 0xBF -> decode_fixstr(bytes)
    0xD9 -> decode_str8(bytes)
    0xDA -> decode_str16(bytes)
    0xDB -> decode_str32(bytes)

    // BINARY
    0xC4 -> decode_bin8(bytes)
    0xC5 -> decode_bin16(bytes)
    0xC6 -> decode_bin32(bytes)

    // ARRAY
    t if t >= 0x90 && t <= 0x9F -> decode_fixarray(bytes)
    0xDC -> decode_array16(bytes)
    0xDD -> decode_array32(bytes)

    // MAP
    t if t >= 0x80 && t <= 0x8F -> decode_fixmap(bytes)
    0xDE -> decode_map16(bytes)
    0xDF -> decode_map32(bytes)
    //
    //    // EXTENDED
    //    0xD4 -> decode_fixext1(bytes)
    //    0xD5 -> decode_fixext2(bytes)
    //    0xD6 -> decode_fixext4(bytes)
    //    0xD7 -> decode_fixext8(bytes)
    //    0xD8 -> decode_fixext16(bytes)
    //    0xC7 -> decode_ext8(bytes)
    //    0xC8 -> decode_ext16(bytes)
    //    0xC9 -> decode_ext32(bytes)
    // FALLBACK
    _ ->
      Error(UnableToDecode(
        "Unsupported or unknown MsgPack tag: " <> int.to_string(tag),
      ))
  }
}

/// Decode one MsgPack value that begins `offset` bits into `bits`.
/// Returns the value and the bit-offset immediately *after* it.
pub fn decode_msgpack_from_bit_offset(
  bits: BitArray,
  offset: Int,
) -> Result(#(MsgPack, Int), DecodeError) {
  case bits {
    <<_skip:bits-size(offset), tail:bits>> ->
      case tail {
        <<tag:unsigned-size(8), _rest:bits>> ->
          decode_value_with_offset(tag, tail, offset)
        _ ->
          Error(UnableToDecode(
            "Bit array too short to contain tag after prefix",
          ))
      }
    _ -> Error(UnableToDecode("Bit array shorter than requested offset"))
  }
}

/// Decode a *fixmap* (0x80‥0x8F). The low nibble is the pair-count.
fn decode_fixmap(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<tag:unsigned-size(8), _rest:bits>> if tag >= 0x80 && tag <= 0x8F -> {
      let count = tag - 0x80
      decode_map_items(bits, 8, count)
    }
    _ -> Error(UnableToDecode("Invalid fixmap tag"))
  }
}

/// Decode *map16* (0xDE) :  1-byte tag  +  2-byte count  +  payload
fn decode_map16(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), count:unsigned-size(16), _rest:bits>> -> {
      decode_map_items(bits, 24, count)
    }
    _ -> Error(UnableToDecode("Invalid map16 header"))
  }
}

/// Decode *map32* (0xDF) :  1-byte tag  +  4-byte count  +  payload
fn decode_map32(bits: BitArray) -> Result(MsgPack, DecodeError) {
  case bits {
    <<_tag:unsigned-size(8), count:unsigned-size(32), _rest:bits>> -> {
      decode_map_items(bits, 40, count)
    }
    _ -> Error(UnableToDecode("Invalid map32 header"))
  }
}

fn decode_map_items(
  bits: BitArray,
  start_bits: Int,
  count: Int,
) -> Result(MsgPack, DecodeError) {
  decode_map_loop(bits, start_bits, count, [])
}

fn decode_map_loop(
  bits: BitArray,
  at_bits: Int,
  remain: Int,
  acc: List(#(MsgPack, MsgPack)),
) -> Result(MsgPack, DecodeError) {
  case remain {
    0 ->
      // Finished: reverse accumulator to preserve order
      Ok(MsgMap(list.reverse(acc)))

    _ -> {
      // Decode key
      case decode_msgpack_from_bit_offset(bits, at_bits) {
        Ok(#(key, next_after_key)) ->
          // Decode value immediately after key
          case decode_msgpack_from_bit_offset(bits, next_after_key) {
            Ok(#(value, next_after_val)) ->
              decode_map_loop(bits, next_after_val, remain - 1, [
                #(key, value),
                ..acc
              ])

            Error(e) -> Error(e)
          }

        Error(e) -> Error(e)
      }
    }
  }
}

fn decode_and_add_bits(
  result: Result(MsgPack, DecodeError),
  bits_used: Int,
) -> Result(#(MsgPack, Int), DecodeError) {
  case result {
    Ok(val) -> Ok(#(val, bits_used))
    Error(e) -> Error(e)
  }
}

fn decode_value_with_consumed_bits(
  tag: Int,
  bits: BitArray,
) -> Result(#(MsgPack, Int), DecodeError) {
  case tag {
    // NIL
    0xC0 -> Ok(#(MsgNil, 8))

    // BOOL
    0xC2 -> Ok(#(MsgBool(False), 8))
    0xC3 -> Ok(#(MsgBool(True), 8))

    // POSITIVE FIXINT
    t if t >= 0x00 && t <= 0x7F -> Ok(#(MsgInt(t), 8))

    // NEGATIVE FIXINT
    t if t >= 0xE0 && t <= 0xFF -> Ok(#(MsgInt(int.subtract(t, 256)), 8))

    // UINT
    0xCC -> decode_and_add_bits(decode_uint8(bits), 16)
    0xCD -> decode_and_add_bits(decode_uint16(bits), 24)
    0xCE -> decode_and_add_bits(decode_uint32(bits), 40)
    0xCF -> decode_and_add_bits(decode_uint64(bits), 72)

    // INT
    0xD0 -> decode_and_add_bits(decode_int8(bits), 16)
    0xD1 -> decode_and_add_bits(decode_int16(bits), 24)
    0xD2 -> decode_and_add_bits(decode_int32(bits), 40)
    0xD3 -> decode_and_add_bits(decode_int64(bits), 72)

    // FLOAT
    0xCA -> decode_and_add_bits(decode_f32(bits), 40)
    0xCB -> decode_and_add_bits(decode_f64(bits), 72)

    // STRINGS
    t if t >= 0xA0 && t <= 0xBF -> {
      let len = t - 0xA0
      decode_fixstr(bits)
      |> result.map(fn(val) { #(val, 8 + len * 8) })
    }
    0xD9 -> decode_strn_with_len(bits, 8)
    0xDA -> decode_strn_with_len(bits, 16)
    0xDB -> decode_strn_with_len(bits, 32)

    // BINARY
    0xC4 -> decode_bin_with_len(bits, 8)
    0xC5 -> decode_bin_with_len(bits, 16)
    0xC6 -> decode_bin_with_len(bits, 32)

    //    // ARRAYS
    //    t if t >= 0x90 && t <= 0x9F -> decode_array_fixed(bits, t - 0x90, 8)
    //    0xDC -> decode_array_sized(bits, 16)
    //    0xDD -> decode_array_sized(bits, 32)
    //
    //    // MAPS
    //    t if t >= 0x80 && t <= 0x8F -> decode_map_fixed(bits, t - 0x80, 8)
    //    0xDE -> decode_map_sized(bits, 16)
    //    0xDF -> decode_map_sized(bits, 32)
    //
    //    // EXT (unsupported for now)
    //    0xD4 -> Error("fixext1 decoding not implemented")
    //    0xD5 -> Error("fixext2 decoding not implemented")
    //    0xD6 -> Error("fixext4 decoding not implemented")
    //    0xD7 -> Error("fixext8 decoding not implemented")
    //    0xD8 -> Error("fixext16 decoding not implemented")
    //    0xC7 -> Error("ext8 decoding not implemented")
    //    0xC8 -> Error("ext16 decoding not implemented")
    //    0xC9 -> Error("ext32 decoding not implemented")
    _ ->
      Error(UnableToDecode(
        "Unsupported or unknown MsgPack tag: " <> int.to_string(tag),
      ))
  }
}

fn drop_offset(result: #(MsgPack, Int)) -> MsgPack {
  let #(mp, _) = result
  mp
}

fn mp_array_to_dynamic(
  values: List(MsgPack),
) -> Result(dynamic.Dynamic, DecodeError) {
  result.map(result.all(list.map(values, mp_to_dynamic)), dynamic.list)
}

fn convert_map_entry(
  entry: #(MsgPack, MsgPack),
) -> Result(#(dynamic.Dynamic, dynamic.Dynamic), DecodeError) {
  let #(k, v) = entry
  result.try(mp_to_dynamic(k), fn(k_) {
    result.map(mp_to_dynamic(v), fn(v_) { #(k_, v_) })
  })
}

fn mp_map_to_dynamic(
  entries: List(#(MsgPack, MsgPack)),
) -> Result(dynamic.Dynamic, DecodeError) {
  result.map(
    result.all(list.map(entries, convert_map_entry)),
    dynamic.properties,
  )
}

fn mp_to_dynamic(mp: MsgPack) -> Result(dynamic.Dynamic, DecodeError) {
  case mp {
    MsgNil -> Ok(dynamic.nil())

    MsgBool(b) -> Ok(dynamic.bool(b))

    MsgInt(i) -> Ok(dynamic.int(i))

    MsgFloat(f) -> Ok(dynamic.float(f))

    MsgString(s) -> Ok(dynamic.string(s))

    MsgBinary(bytes) -> Ok(dynamic.bit_array(bytes))

    MsgArray(values) -> mp_array_to_dynamic(values)

    MsgMap(entries) -> mp_map_to_dynamic(entries)
  }
}

fn parse_bits(bits: BitArray) -> Result(dynamic.Dynamic, DecodeError) {
  decode_msgpack_from_bit_offset(bits, 0)
  |> result.map(drop_offset)
  // Result(MsgPack, DecodeError)
  |> result.try(mp_to_dynamic)
  // Result(Dynamic, String)
}

fn decode_path_to_string(path: List(String)) -> String {
  path
  |> list.reverse
  |> string.join("/")
}

fn convert_decode_errors(errors: List(decode.DecodeError)) -> DecodeError {
  case errors {
    [decode.DecodeError(expected, found, path), ..] -> {
      let reason =
        "Expected "
        <> expected
        <> ", but found "
        <> found
        <> case path {
          [] -> ""
          _ -> " at " <> decode_path_to_string(path)
        }
      UnableToDecode(reason)
    }

    [] -> UnableToDecode("Unknown decode error")
  }
}

pub fn parse(
  from bits: BitArray,
  using decoder: decode.Decoder(t),
) -> Result(t, DecodeError) {
  use dynamic <- result.try(parse_bits(bits))

  decode.run(dynamic, decoder)
  |> result.map_error(convert_decode_errors)
}
