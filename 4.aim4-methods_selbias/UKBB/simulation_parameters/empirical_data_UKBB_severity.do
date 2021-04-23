/*

A. R. Carter - 23/04/2021
Generating parameter estimates in UK Biobank to inform simulations of COVID-19 severity in UK Biobank
This script uses data generated on the 07/04/2021, including COVID-19 test results until February 2021. 
These simulations focus only on the pre-mass testing phase of the pandemic

*/

use "$resDir/data/COVIDITY/UKBB/covidity_data_202104.dta", clear
cd "$resDir/results/COVIDITY/UKBB/for_simulations/paper_2"

/*
Analysis/code notes:

Restrict analysis dates to pre mass testing variables as defined in defining_covid_cases.do and generate_data_for_phase.do (18th May 2020)

N = 421,037

Cases/tests pre mass testing have the suffix _phase1 - note that this definition is based on testing dates where phase1 means the pre-mass testing phase, until the 28th May

Simulations are currently run on case definition:
1. Using both tests and cases to define COVID-19 cases (i.e., a case can be a participant with a COVID-19 death but with no test)


*/

* Case definitionusing test and death data - Phase 1 (pre-mass testing)
do "$scriptDir/4.aim4-methods_selbias/UKBB/simulation_parameters/severity_simulation_parameters.do" "TestsAndDeath" "death" "phase1" "20210423"

* Case definition using test and death data - Phase 2 (post-mass testing)
do "$scriptDir/4.aim4-methods_selbias/UKBB/simulation_parameters/severity_simulation_parameters.do" "TestsAndDeath" "death" "phase2" "20210423"
