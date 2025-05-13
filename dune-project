(lang dune 3.10)
(name TCSLib)
(version 0.5)

(generate_opam_files true)

(source (github tcsprojects/tcslib))

(package
 (name TCSLib)
 (synopsis "A multi-purpose library for OCaml")
 (description "A multi-purpose library for OCaml.")
 (depends
   (ocaml (>= 4.8))
   dune
   num
 )
 (license "BSD-3-clause")
 (authors "Oliver Friedmann" "Martin Lange") 
 (maintainers "Oliver Friedmann" "Martin Lange")
 (homepage "https://github.com/tcsprojects/tcslib")
 (bug_reports "https://github.com/tcsprojects/tcslib/issues")
)
