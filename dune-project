(lang dune 3.24)

(name compiler)

(generate_opam_files true)

(source
 (github mp2720/compiler))

(authors "Salomakhin Timur <mp2720@proton.me>")

(package
 (name compiler)
 (depends ocaml))

(using menhir 3.0)
