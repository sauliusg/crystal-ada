#!/bin/bash

# Test the data roundtrip:

diff -s \
     <(./bin/xyz2ortho \
           --cell "6.1203 13.7779 13.4435 90 99.229 90" \
           tests/data/2000001.fxyz \
           | ./bin/xyz2fract -f 3,5,0 \
       ) \
     <(./bin/xyz2ortho \
           --cell "6.1203 13.7779 13.4435 90 99.229 90" \
           tests/data/2000001.fxyz \
           | ./bin/xyz2fract -f 3,5,0 \
           | ./bin/xyz2ortho \
           | ./bin/xyz2fract -f 3,5,0
      ) \
          | sed 's/Files .* are identical/Files are identical/'
