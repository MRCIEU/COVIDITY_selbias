#!/bin/bash
#PBS -l walltime=5:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-null-bmionly.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR

module add apps/stata15

stata sim-infection.do bmi 2 "null"

date

