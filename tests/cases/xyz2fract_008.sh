#!/bin/sh

./bin/xyz2ortho \
    --cell "6.1203 13.7779 13.4435 90 99.229 90" \
    tests/data/2000001.fxyz \
| ./bin/xyz2fract -f 5,6,0
