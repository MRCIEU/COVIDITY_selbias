#!/bin/bash
#PBS -l walltime=8:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bmieffect-extreme.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ..

module add apps/stata15

stata sim-severity.do "effect" "extreme"

date

