/* 

Alice R. Carter - 04/03/2021
Testing the association between UKBB participant characteristics and obtaining a covid test

Here, we are interested in the association between BMI, sex and smoking and COVID-19 testing, infection and severity

We are interested in the pre-mass testing (phase 1) and post mass testing (phase 2) periods separately. Mass testing here is defined as the wide scale population testing available on or after the 18th May

This main do file calls a sub-do file which carries out analyses stratified by pre/post mass testing and uses two case definitions 
1. Tests only (not accounting for deaths without a positive test)
2. Tests and/or COVID deaths (U07.1 or U07.2 on a death certificate)

*/

* Open the data and change the working directory
use "$resDir/data/COVIDITY/UKBB/covidity_data_202104.dta"
cd "$resDir/results/COVIDITY/UKBB/multivariate/202104"

* Set sample to have complete data on all exposures/covariates
capture drop touse
mark touse 
markout touse sd_age bmi current_smoke sd_bmi eduyears_quali tdi_cat
keep if touse==1

*** INFECTION ***
*** Sex combined ***
* Phase 1 results, accounting for tests only
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_infection.do" "phase1" "test" "tested"   "20210414"
* Phase 1 results, accounting for tests/deaths
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_infection.do" "phase1" "death"  "TestsAndDeath" "20210414"
* Phase 1 results, accounting for tests only
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_infection.do" "phase2" "test" "tested"   "20210414"
* Phase 1 results, accounting for tests/deaths
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_infection.do" "phase2" "death"  "TestsAndDeath" "20210414"


*** Severity ***
*** Sex combined ***
* Phase 1 results, accounting for deaths with a test only
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_severity.do" "phase1" "testonly_" "tested"   "20210414"
* Phase 1 results, accounting for deaths regardless of tests
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_severity.do" "phase1" "" "TestsAndDeath" "20210414"
* Phase 1 results, accounting for deaths with a test only
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_severity.do" "phase2" "testonly_" "tested"   "20210414"
* Phase 1 results, accounting for deaths regardless of tests
do "$scriptDir/3.aim3-examples/UKBB/sub_multivariate_severity.do" "phase2" "" "TestsAndDeath" "20210414"
