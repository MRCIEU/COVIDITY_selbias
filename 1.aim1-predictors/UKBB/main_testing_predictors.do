/* 

Alice R. Carter - 04/03/2021
Testing the association between UKBB participant characteristics and obtaining a covid test

Here, we are interested in the effect of a range of behavioural, socio-demographic and health related variables on obtaining a covid test

The four outcome comparisons are interest are:
1. Who gets a test (tested vs non-tested)
2. What factors are potentially indicating selection (test negative vs non-tested)
3. What factors might be associated with COVID infection or selection (test positive vs non-tested)
4. Potential risk factors for COVID-19 (test positive vs test negative)

We are interested in the pre-mass testing (phase 1) and post mass testing (phase 2) periods separately. Mass testing here is defined as the wide scale population testing available on or after the 18th May

This main do file calls a sub-do file which carries out analyses stratified by pre/post mass testing and uses two case definitions 
1. Tests only (not accounting for deaths without a positive test)
2. Tests and/or COVID deaths (U07.1 or U07.2 on a death certificate)

*/

* Open the data and change the working directory
use "$resDir/data/COVIDITY/UKBB/covidity_data_202104.dta"
cd "$resDir/results/COVIDITY/UKBB/univariate/202104"


*** INFECTION ***
* Phase 1 results, accounting for tests only
do "$scriptDir/1.aim1-predictors/UKBB/sub_testing_predictors.do" "phase1" "test" "tested"   "20210413"

* Phase 1 results, accounting for tests/deaths
do "$scriptDir/1.aim1-predictors/UKBB/sub_testing_predictors.do" "phase1" "death"  "TestsAndDeath" "20210413"

* Phase 1 results, accounting for tests only
do "$scriptDir/1.aim1-predictors/UKBB/sub_testing_predictors.do" "phase2" "test" "tested"   "20210413"

* Phase 1 results, accounting for tests/deaths
do "$scriptDir/1.aim1-predictors/UKBB/sub_testing_predictors.do" "phase2" "death"  "TestsAndDeath" "20210413"


*** Severity ***
* Phase 1 results, accounting for deaths with a test only
do "$scriptDir/1.aim1-predictors/UKBB/sub_severity_predictors.do" "phase1" "testonly_" "tested"   "20210413"

* Phase 1 results, accounting for deaths regardless of tests
do "$scriptDir/1.aim1-predictors/UKBB/sub_severity_predictors.do" "phase1" "" "TestsAndDeath" "20210413"

* Phase 1 results, accounting for deaths with a test only
do "$scriptDir/1.aim1-predictors/UKBB/sub_severity_predictors.do" "phase2" "testonly_" "tested"   "20210413"

* Phase 1 results, accounting for deaths regardless of tests
do "$scriptDir/1.aim1-predictors/UKBB/sub_severity_predictors.do" "phase2" "" "TestsAndDeath" "20210413"
