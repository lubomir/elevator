#!/bin/bash

set -ex

#for f in 5 8 10 12 15 20; do
for f in $(seq 0.01 0.005 0.4 | tr ',' '.'); do
    for t in dumb nice smart; do
        ./dist/build/elevator/elevator $t $f >$t-$f.csv
    done
    Rscript render.r $f
done

