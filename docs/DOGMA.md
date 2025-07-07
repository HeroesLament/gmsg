# DOGMA — Design Principles of `gmsg`

1. **Single pair of public entry-points.**  
   `encode(msg_pack)` turns a `MsgPack` value into a `BitArray`; `parse(bits, using decoder)` turns a `BitArray` back into a typed value via a `dynamic.Decoder`.

2. **Three-layer architecture.**  
   *Wire format* ↔︎ **`MsgPack` IR** ↔︎ **`dynamic.Dynamic`** ↔︎ *your domain type*; the middle two layers mirror Gleam’s JSON codec so one decoder can serve all formats.

3. **Deterministic output.**  
   Integers are big-endian, lengths are canonical, and arrays/maps preserve caller order, guaranteeing byte-stable hashes and signatures.

4. **Offset-aware streaming.**  
   The parser returns `(value, next_bit)` tuples internally, letting higher-level helpers walk large documents without copying.

5. **Backend agnostic.**  
   Pure Gleam: works unchanged on Erlang or JavaScript, needs no NIFs, and allocates minimally.