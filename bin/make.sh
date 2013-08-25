#!/bin/sh

fo=`basename $1 .hs`
dir=`dirname $1`
inc="../src:$dir"

ghc --make -i$inc $1 -o $fo

ghc --make -i$inc $1 -static -o $fo-static -optl-pthread -optl-static -rtsopts

rm $dir/*.hi
rm $dir/*.o

