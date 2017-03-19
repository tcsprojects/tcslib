#!/usr/bin/env bash
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
oasis2opam --local
opam uninstall TCSLib
opam pin remove TCSLib
opam pin add TCSLib . -n
opam install TCSLib
