#env 1>&2
redo-ifchange redo.hs README.txt
ghc -v0  -o $3 redo.hs