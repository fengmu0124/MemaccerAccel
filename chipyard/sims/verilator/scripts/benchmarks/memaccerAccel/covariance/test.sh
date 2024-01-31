simName="simulator-chipyard-MemaccerAccel-1line"
#simDir="/home/zwk/chipyard/sims/verilator/"
simDir="./"
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/covariance"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

chmod +x simulator-chipyard-MemaccerAccel-1line
make clean
make 

# debug
${sim} +verbose ${bin} 2>${simout}

# ${sim} ${bin}