# diag_manager2
diag_manager rewrite

Instructions for running the diag_manager
1. `git clone --recursive git@github.com:thomas-robinson/diag_manager2.git`
2. `cd diag_manager2/libyaml`
3. `./bootstrap && ./configure && make`
4. `setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${PWD}/src/.libs` 
5. `cd ..`
6. Edit Makefile to have correct compiler, paths, mpi_run command, FFLAGS, etc
7. `make` 


