#!/bin/bash
#PBS -l walltime=2:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-null-10.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR

module add apps/stata15

stata sim-main-null.do all 10

date

