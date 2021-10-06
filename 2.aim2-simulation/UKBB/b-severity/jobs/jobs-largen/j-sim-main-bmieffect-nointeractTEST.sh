#!/bin/bash
#PBS -l walltime=80:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-bmieffect-nointeractTEST.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR
cd ../..

module add apps/stata15

stata sim-severityTEST.do "effect" "nointeract" 1

date

