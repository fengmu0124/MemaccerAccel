simName="simulator-chipyard-MemaccerAccel"
#simDir="/home/zwk/chipyard/sims/verilator/"
simDir="./"
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/axpy"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

chmod +x simulator-chipyard-MemaccerAccel
make clean
make 

# debug
# ${sim} +verbose ${bin} 2>${simout}

${sim} ${bin}