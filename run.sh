# Shell script to compile and run the case

echo "Compiling..."
mkdir -p comp
cd comp
gfortran -c ../src/func_m.f90 ../src/main.f90
gfortran func_m.o main.o

echo "Running the case..."
./a.out
mkdir -p ../data
mv data.dat ../data/
