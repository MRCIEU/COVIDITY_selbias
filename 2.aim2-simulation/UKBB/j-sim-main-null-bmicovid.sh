#!/bin/bash
#PBS -l walltime=2:00:00,nodes=1:ppn=1
#PBS -o output-sim-main-null-bmicovid.file
#---------------------------------------------

date

cd $PBS_O_WORKDIR

module add apps/stata15

stata -b sim-main-null.do bmi_covid

date

