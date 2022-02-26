#!/bin/sh

# saulius@starta cases/ $ grep ^_cell_ $(codid2file 2000000)
# _cell_angle_alpha                111.64(1)
# _cell_angle_beta                 107.94(1)
# _cell_angle_gamma                93.45(1)
# _cell_formula_units_Z            1
# _cell_length_a                   8.243(2)
# _cell_length_b                   10.368(3)
# _cell_length_c                   11.497(3)
# _cell_volume                     851.974

./bin/xyz2fract tests/data/2000000.xyz --cell "8.243 10.368 11.497 111.64 107.94 93.45"
