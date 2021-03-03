#!/bin/bash
#PBS -l walltime=5:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-effect.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR

module add apps/stata15

stata sim-main-effect.do all 2

date

