#! /bin/bash
./oasis_cleanup.sh
./oasis_gen.sh
./configure --enable-tests
make
./test_using_z_alg.native
