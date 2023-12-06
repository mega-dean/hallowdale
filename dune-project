(lang dune 3.0)

(name hallowdale)

(generate_opam_files true)

(source
 (github mega-dean/hallowdale))

(package
 (name hallowdale)
 (synopsis "An homage to Hollow Knight")
 (depends
  atdgen
  dune
  raylib
  yojson
  )
 )

(authors "mega-dean")
(maintainers "mega-dean")

(license "MIT (code), CC-BY-4.0 (assets)")
