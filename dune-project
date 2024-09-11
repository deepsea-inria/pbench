(lang dune 3.11)
(generate_opam_files true)

(package
  (name pbench_xlib)
  (version 1.0)
  (authors
    "Arthur Charguéraud <arthur.chargueraud@inria.fr>"
    "Mike Rainey <me@mike-rainey.site>"
  )
  (maintainers
    "Arthur Charguéraud <arthur.chargueraud@inria.fr>"
  )
  (source (uri git+https://github.com/deepsea-inria/pbench.git))
  (homepage https://github.com/deepsea-inria/pbench)
  (bug_reports https://github.com/deepsea-inria/pbench/-/issues)
  (synopsis "An eXtension library")
  (license "Apache-2.0")
  (depends
    ocaml
    dune
  )
)

(package
  (name pbench)
  (version 1.0)
  (authors
    "Arthur Charguéraud <arthur.chargueraud@inria.fr>"
    "Mike Rainey <me@mike-rainey.site>"
  )
  (maintainers
    "Arthur Charguéraud <arthur.chargueraud@inria.fr>"
  )
  (source (uri git+https://github.com/deepsea-inria/pbench.git))
  (homepage https://github.com/deepsea-inria/pbench)
  (bug_reports https://github.com/deepsea-inria/pbench/-/issues)
  (synopsis "A parallel algorithm benchmarking toolkit")
  (license "Apache-2.0")
  (depends
    ocaml
    dune
    xlib
  )
)
