#!/bin/bash
#PBS -l walltime=100:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bminull-plausible-largen.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ../..

module add apps/stata15

stata sim-severity.do "null" "plausible" 1

date

