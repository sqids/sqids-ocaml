(lang dune 3.13)

(name sqids)

(generate_opam_files true)

(source
 (github sqids.org/ocaml))

(authors "Leo Soares")

(maintainers "Leo Soares" "Rizo Isrof")

(license MIT)

(documentation https://sqids.org/ocaml)

(package
 (name sqids)
 (synopsis "OCaml port of Sqids")
 (description "Generate short unique IDs from numbers.")
 (depends
   ocaml
   dune
   (qcheck-core :with-test)
   (ocaml-lsp-server :with-dev-setup)
   (ocamlformat :with-dev-setup)))
