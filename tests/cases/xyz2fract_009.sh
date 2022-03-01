#!/bin/sh

./bin/xyz2ortho \
    --cell "8.243 10.368 11.497 111.64 107.94 93.45" \
    tests/data/2000000.fxyz \
| ./bin/xyz2fract -f 5,6,0
