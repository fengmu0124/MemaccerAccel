simName="simulator-chipyard-MemaccerAccel-test"
simDir="/home/zwk/chipyard/sims/verilator/"
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/gemm-opt"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

chmod +x simulator-chipyard-MemaccerAccel-1line
make clean
make 

# debug
${sim} +verbose ${bin} 2>${simout}

# ${sim} ${bin}