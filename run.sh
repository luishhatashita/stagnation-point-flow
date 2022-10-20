# Shell script to compile and run the case

echo "Compiling..."
mkdir -p comp
cd comp
gfortran -c ../src/func_m.f90 ../src/ode_solver_m.f90 ../src/main.f90
gfortran func_m.o ode_solver_m.o main.o

echo "Running the case..."
./a.out
mkdir -p ../data
mv stag_flow.csv ../data/
