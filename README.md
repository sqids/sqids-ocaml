# [Sqids OCaml](https://sqids.org/ocaml)

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ocaml.ci.dev/badge/sqids/sqids-ocaml/main&logo=ocaml)](https://ocaml.ci.dev/github/sqids/sqids-ocaml)

[Sqids](https://sqids.org/ocaml) (*pronounced "squids"*) is a small library that lets you **generate unique IDs from numbers**. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

Features:

- **Encode multiple numbers** - generate short IDs from one or several non-negative numbers
- **Quick decoding** - easily decode IDs back into numbers
- **Unique IDs** - generate unique IDs by shuffling the alphabet once
- **ID padding** - provide minimum length to make IDs more uniform
- **URL safe** - auto-generated IDs do not contain common profanity
- **Randomized output** - Sequential input provides nonconsecutive IDs
- **Many implementations** - Support for [40+ programming languages](https://sqids.org/)

## 🧰 Use-cases

Good for:

- Generating IDs for public URLs (eg: link shortening)
- Generating IDs for internal systems (eg: event tracking)
- Decoding for quicker database lookups (eg: by primary keys)

Not good for:

- Sensitive data (this is not an encryption library)
- User IDs (can be decoded revealing user count)

## 🚀 Getting started

Install:

```shell
opam install sqids
```

Add to `dune-project`:

```text
(sqids (= 0.1.0))
```

Add to `dune` for your target:

```text
(libraries sqids)
```

Try in `utop`:

```ocaml
# require "sqids";;
let s = Sqids.make () in
Sqids.encode s [1; 2; 3]
```

## 👩‍💻 Examples

Simple encode & decode:

> **Note**
> 🚧 Because of the algorithm's design, **multiple IDs can decode back into the same sequence of numbers**. If it's important to your design that IDs are canonical, you have to manually re-encode decoded numbers and check that the generated ID matches.

Enforce a *minimum* length for IDs:

```ocaml
let sqids = Sqids.make ~min_length:10 () in
let id = Sqids.encode sqids [1; 2; 3] in (* "86Rf07xd4z" *)
let numbers = Sqids.decode sqids id in (* [1; 2; 3] *)
```

Randomize IDs by providing a custom alphabet:

```ocaml
let sqids = Sqids.make ~alphabet:"FxnXM1kBN6cuhsAvjW3Co7l2RePyY8DwaU04Tzt9fHQrqSVKdpimLGIJOgb5ZE" () in
let id = Sqids.encode sqids [1; 2; 3] in (* "B4aajs" *)
let numbers = Sqids.decode sqids id in (* [1; 2; 3] *)
```

Prevent specific words from appearing anywhere in the auto-generated IDs:

```ocaml
let sqids = Sqids.make ~blocklist:["86Rf07"] () in
let id = Sqids.encode sqids [1; 2; 3] in (* "se8ojk" *)
let numbers = Sqids.decode sqids id in (* [1; 2; 3] *)
```

## 📝 License

[MIT](LICENSE)
