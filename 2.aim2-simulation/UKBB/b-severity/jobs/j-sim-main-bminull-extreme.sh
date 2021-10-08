#!/bin/bash
#PBS -l walltime=8:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bminull-extreme.file
#PBS -t 1-20 
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ..

module add apps/stata15

stata sim-severity.do "null" "extreme" 0 ${PBS_ARRAYID}

date

