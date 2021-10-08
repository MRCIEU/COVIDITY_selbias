#!/bin/bash
#PBS -l walltime=10:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bmieffect-plausible-largen.file
#PBS -t 1-20 
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ../..

module add apps/stata15

stata sim-severity.do "effect" "plausible" 1 ${PBS_ARRAYID}

date

