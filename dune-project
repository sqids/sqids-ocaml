(lang dune 3.13)

(name sqids)

(generate_opam_files true)

(source
 (github sqids/sqids-ocaml))

(authors "Leo Soares")

(maintainers "Leo Soares" "Rizo Isrof")

(license MIT)

(documentation https://sqids.org/ocaml)

(package
 (name sqids)
 (synopsis "OCaml port of Sqids")
 (description "Generate short unique IDs from numbers.")
 (depends
  (ocaml
   (>= 4.11.0))
  dune
  (qcheck-core
   (and
    :with-test
    (>= 0.21.3)))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))))
