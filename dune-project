(lang dune 3.10)

(name vrosi)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Canrad")

(maintainers "Canrad")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name vrosi)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
