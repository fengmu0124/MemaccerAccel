simName="simulator-chipyard-MemaccerAccel"
# simDir="/home/zwk/chipyard/sims/verilator/"
simDir="./"
chmod +x simulator-chipyard-MemaccerAccel
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/gemm"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

make clean
make 
# debug
# ${sim} +verbose ${bin} 2>${simout}

${sim} ${bin}