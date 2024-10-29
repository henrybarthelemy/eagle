(lang dune 3.16)

(using menhir 3.0)

(name eagle)

(generate_opam_files true)

(package
 (name eagle)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving menhir sedlex)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
