cd ..
mkdir build
cd build
FC=pgf90 cmake ..
make
make test
