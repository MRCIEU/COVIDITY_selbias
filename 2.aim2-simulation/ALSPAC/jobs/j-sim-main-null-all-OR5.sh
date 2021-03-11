#!/bin/bash
#PBS -l walltime=5:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-null-5.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd .. 

module add apps/stata15

stata sim-main-null.do all 5

date

