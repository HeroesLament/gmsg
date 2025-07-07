# gmsg examples — direct MsgPack IR

Below are three short Gleam snippets that build `MsgPack` values by hand, pass them to `gmsg.encode/1`, and (optionally) round-trip with `decode_msgpack_from_bit_offset/2`.  
All of them avoid the **dynamic** layer so you can see the raw IR helpers in action.

---

## 1 · Single user record (map)

```gleam
import gmsg
import gleam/bit_array.{inspect}

let user =
  gmsg.map(
    from: [#("id", 42), #("name", "Ada"), #("active", True)],
    of: gmsg.pack_string,
    and: fn
      Int(i) -> gmsg.pack_int(i)
      String(s) -> gmsg.pack_string(s)
      Bool(b) -> gmsg.pack_bool(b)
      _ -> gmsg.pack_nil()
  )

let Ok(bin) = gmsg.to_bit_array(user)
inspect(bin) |> io.println
```

## 2 · List of users
```
import gmsg

let users =
  gmsg.array(
    from: [
      gmsg.map(
        from: [#("id", 1), #("name", "Ada")],
        of: gmsg.pack_string,
        and: gmsg.pack_int
      ),
      gmsg.map(
        from: [#("id", 2), #("name", "Grace")],
        of: gmsg.pack_string,
        and: gmsg.pack_int
      )
    ],
    of: fn(x) { x } // elements already MsgPack
  )

let Ok(payload) = gmsg.to_bit_array(users)
```

## 3 · Round-trip sanity check
```
import gmsg
import gleam/result

let original = gmsg.pack_string("hello, MsgPack!")
let Ok(bits) = gmsg.to_bit_array(original)

case gmsg.decode_msgpack_from_bit_offset(bits, 0) {
  Ok(#(value, _)) -> assert value == original
  Error(e) -> io.println("decode error: " <> e)
}
```