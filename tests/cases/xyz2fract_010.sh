#!/bin/bash

# Test the data roundtrip:

diff -s \
     <(./bin/xyz2fract \
           --cell "6.1203 13.7779 13.4435 90 99.229 90" \
           tests/data/2000001.xyz \
           | ./bin/xyz2ortho -f 3,5,0 \
       ) \
     <(./bin/xyz2fract \
           --cell "6.1203 13.7779 13.4435 90 99.229 90" \
           tests/data/2000001.xyz \
           | ./bin/xyz2ortho -f 3,5,0 \
           | ./bin/xyz2fract \
           | ./bin/xyz2ortho -f 3,5,0
      ) \
          | sed 's/Files .* are identical/Files are identical/'
