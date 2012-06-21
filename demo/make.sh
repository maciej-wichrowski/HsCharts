#!/bin/sh

if [ ! -d bin ]
then
    mkdir bin
fi

ghc --make Main.hs -o demo \
                   -outputdir bin \
                   -O2 \
                   -threaded \
                   -i.:./../:./../../:./../../../
