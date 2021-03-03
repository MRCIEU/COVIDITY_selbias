#!/bin/bash
#PBS -l walltime=5:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-null-bmicovars.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR

module add apps/stata15

stata sim-main-null.do bmi_covars

date

