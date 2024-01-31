simName="simulator-chipyard-MemaccerAccel-4line"
#simDir="/home/zwk/chipyard/sims/verilator/"
simDir="./"
sim=${simDir}${simName}

binDir="/home/zwk/chipyard/sims/verilator/scripts/benchmarks/memaccerAccel/kmean"
bin="${binDir}/main.riscv"
simout="${binDir}/simout"

chmod +x sim
make clean
make 

# debug
${sim} +verbose ${bin} 2>${simout}

#${sim} ${bin}