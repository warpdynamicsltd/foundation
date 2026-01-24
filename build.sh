#!/bin/bash
eval $(opam env)
dune build || exit 1
dune test || exit 1
rm -f foundation
cp _build/install/default/bin/foundation foundation
