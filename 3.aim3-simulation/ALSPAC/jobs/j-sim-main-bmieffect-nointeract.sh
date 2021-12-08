#!/bin/bash
#PBS -l walltime=2:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bmieffect-nointeract.file
#PBS -t 1-20 
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ..

module add apps/stata15

stata sim-infection.do "effect" "nointeract" 0 ${PBS_ARRAYID}

date

