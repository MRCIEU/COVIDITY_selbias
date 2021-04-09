/*

A. R. Carter - 09/04/2021
Generating parameter estimates in UK Biobank to inform simulations of COVID-19 infection in UK Biobank
This script uses data generated on the 07/04/2021, including COVID-19 test results until February 2021. 

These simulations focus only on the pre-mass testing phase of the pandemic

*/

use "$resDir/data/COVIDITY/UKBB/covidity_data_202104.dta", clear
cd "$resDir/results/COVIDITY/UKBB/for_simulations"

/*
Analysis/code notes:

Restrict analysis dates to pre mass testing variables as defined in defining_covid_cases.do and generate_data_for_phase.do (18th May 2020)

N = 421,037

Cases/tests pre mass testing have the suffix _phase1 - note that this definition is based on testing dates where phase1 means the pre-mass testing phase, until the 28th May

Two sets of data are created for the simulations:
1. Using tested cases only (i.e., not accounting for COVID-19 deaths *without* a COVID-19 test)
2. Using both tests and cases to define COVID-19 cases (i.e., a case can be a participant with a COVID-19 death but with no test)

*/

* Only keep participants eligible to be included in the pre-mass testing sample
keep if phase1_sample==1

* Case definition (1) using test data only
do "$scriptDir/2.aim2-simulation/UKBB/a-infection/infection_simulation_parameters.do" "tested" "test"

* Case definition (2) using test and death data
do "$scriptDir/2.aim2-simulation/UKBB/a-infection/infection_simulation_parameters.do" "TestsAndDeath" "death"
