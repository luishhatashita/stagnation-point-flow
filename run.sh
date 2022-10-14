# Shell script to compile and run the case

echo "Compiling..."
mkdir -p comp
cd comp
gfortran -c ../src/func_mod.f90 ../src/4_runge_kutta.f90
gfortran func_mod.o 4_runge_kutta.o

echo "Running the case..."
./a.out
mkdir -p ../data
mv data.dat ../data/
