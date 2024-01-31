simName="simulator-chipyard-MemaccerAccel-1line"
simDir="/home/zwk/chipyard/sims/verilator/"
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/base"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

make clean
make 

# debug
${sim} +verbose ${bin} 2>${simout}

# ${sim} ${bin}