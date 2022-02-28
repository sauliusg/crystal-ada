#!/bin/sh

cat \
    tests/data/2000000.xyz \
    tests/data/2000001.xyz \
| ./bin/xyzformat \
    --float-format 8,6,0
