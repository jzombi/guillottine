#!/bin/bash

compiler=fpc
unitpath="../"
flags="-glh"

for src in $(ls test_*.pas); do
    ${compiler} ${flags} -Fu${unitpath} ${src}
done;

rm *.o *.ppu