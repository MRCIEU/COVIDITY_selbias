#!/bin/bash
#PBS -l walltime=8:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bmieffect-plausible.file
#PBS -t 1-20 
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ..

module add apps/stata15

stata sim-infection.do "effect" "plausible" 0 ${PBS_ARRAYID}

date

