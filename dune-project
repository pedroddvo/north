(lang dune 3.4)

(name north)

(generate_opam_files true)

(source
 (github pedroddvo/north))

(authors "Pedro Oliveira")

(maintainers "Pedro Oliveira")

(using menhir 2.1)

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name north)
 (synopsis "A concatenative, functional, pattern based programming language")
 (description "A concatenative, functional, pattern based programming language")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
