import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleeunit
import gleeunit/should

import gmsg

pub fn encode_float_test() {
  gmsg.encode(gmsg.MsgFloat(3.14))
  |> should.equal(Ok(<<0xCB, 64, 9, 30, 184, 81, 235, 133, 31>>))
}

pub fn encode_string_test() {
  gmsg.encode(gmsg.MsgString("ok"))
  |> should.equal(Ok(<<0xA2, 111, 107>>))
}

pub fn encode_binary_test() {
  gmsg.encode(gmsg.MsgBinary(<<1, 2, 3>>))
  |> should.equal(Ok(<<0xC4, 3, 1, 2, 3>>))
}

pub fn encode_array_test() {
  gmsg.encode(gmsg.MsgArray([gmsg.MsgInt(1), gmsg.MsgInt(2)]))
  |> should.equal(Ok(<<0x92, 1, 2>>))
}

pub fn encode_map_test() {
  gmsg.encode(gmsg.MsgMap([#(gmsg.MsgString("a"), gmsg.MsgInt(5))]))
  |> should.equal(Ok(<<0x81, 0xA1, 97, 5>>))
}

pub fn decode_string_test() {
  gmsg.parse(<<0xA2, 111, 107>>, decode.string)
  |> should.equal(Ok("ok"))
}

fn build_a_decoder(value: dynamic.Dynamic) -> decode.Decoder(BitArray) {
  case decode.run(value, decode.bit_array) {
    Ok(bytes) -> decode.success(bytes)
    Error(_) -> {
      let actual_type = dynamic.classify(value)
      decode.failure(<<>>, "Expected BitArray, got " <> actual_type)
    }
  }
}

pub fn decode_binary_test() {
  let binary_decoder = decode.then(decode.dynamic, build_a_decoder)

  gmsg.parse(<<0xC4, 3, 1, 2, 3>>, binary_decoder)
  |> should.equal(Ok(<<1, 2, 3>>))
}

pub fn decode_array_test() {
  // list(Int) → Decoder(List(Int))
  let int_array_decoder = decode.list(decode.int)

  gmsg.parse(<<0x92, 1, 2>>, int_array_decoder)
  |> should.equal(Ok([1, 2]))
}

fn dict_to_pairs(kv: dict.Dict(String, Int)) -> List(#(String, Int)) {
  dict.fold(over: kv, from: [], with: fn(acc, k, v) { [#(k, v), ..acc] })
}

pub fn decode_map_test() {
  // step 1 – decode the MsgPack map into Dict(String, Int)
  let dict_decoder = decode.dict(decode.string, decode.int)

  // step 2 – turn Dict into List of pairs so we can compare easily
  let pair_list_decoder = dict_decoder |> decode.map(dict_to_pairs)

  gmsg.parse(<<0x81, 0xA1, 97, 5>>, pair_list_decoder)
  |> should.equal(Ok([#("a", 5)]))
}

pub fn decode_float_test() {
  gmsg.parse(<<0xCB, 64, 9, 30, 184, 81, 235, 133, 31>>, decode.float)
  |> should.equal(Ok(3.14))
}

pub fn decode_fixint_zero_offset_test() {
  gmsg.decode_msgpack_from_bit_offset(<<0x2A:8>>, 0)
  |> should.equal(Ok(#(gmsg.MsgInt(42), 8)))
}

pub fn decode_fixint_midstream_test() {
  let bytes = <<0xFF:8, 0x05:8>>
  // dummy prefix + value
  gmsg.decode_msgpack_from_bit_offset(bytes, 8)
  |> should.equal(Ok(#(gmsg.MsgInt(5), 16)))
}

pub fn decode_fixstr_hi_test() {
  let payload = <<0xA2:8, 0x68:8, 0x69:8>>
  gmsg.decode_msgpack_from_bit_offset(payload, 0)
  |> should.equal(Ok(#(gmsg.MsgString("hi"), 24)))
}

pub fn main() -> Nil {
  gleeunit.main()
}
