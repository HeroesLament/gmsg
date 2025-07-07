# gmsg

[![Package Version](https://img.shields.io/hexpm/v/gmsg)](https://hex.pm/packages/gmsg)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gmsg)
[![CI](https://img.shields.io/github/actions/workflow/status/HeroesLament/gmsg/test.yml?label=CI)](https://github.com/HeroesLament/gmsg/actions/workflows/test.yml)


> MessagePack encoding/decoding for Gleam.

## Install
```sh
gleam add gmsg@1
```

## Quick start
```gleam
import gmsg
import gleam/io

let data = gmsg.pack_string("hello")
let Ok(bin) = gmsg.to_bit_array(data)
io.println(bit_array.inspect(bin))

// returns Dynamic
let Ok(dynamic) =
  gmsg.parse(bin, using: decode.string)
```

Further documentation can be found at <https://hexdocs.pm/gmsg>.

## Development

```sh
gleam test  # Run the tests
```

## License
Apache-2.0 © HeroesLament