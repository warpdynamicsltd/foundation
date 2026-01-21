(lang dune 3.19)
(using menhir 3.0)

(name foundation)

(generate_opam_files true)

(source
 (github warpdynamicsltd/foundation))

(authors "Michal Stanislaw Wojcik")

(maintainers "Michal Stanislaw Wojcik")

(license LICENSE)

(documentation https://github.com/warpdynamicsltd/foundation)

(package
 (name foundation)
 (synopsis "Foundation Library")
 (description "Foundation Library")
 (depends ocaml dune menhir)
 (tags
  (library)))