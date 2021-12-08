/*

A. R. Carter - 22/07/2021
Code to investigate test positivity ad potential interactions between BMI and COVID-19. 

These simulations focus only on the pre-mass testing phase of the pandemic

*/

use "$resDir/data/COVIDITY/UKBB/covidity_data_202104.dta", clear
cd "$resDir/results/COVIDITY/UKBB/for_simulations"

* Only keep participants eligible to be included in the pre-mass testing sample
keep if phase1_sample==1

* Create strata of BMI
capture drop bmi_cat
gen bmi_cat = 1 if bmi_0_0 <18.5 & bmi_0_0!=.
replace bmi_cat = 2 if bmi_0_0 >=18.5 & bmi_0_0 <25 & bmi_0_0!=.
replace bmi_cat = 3 if bmi_0_0 >=25 & bmi_0_0 <30 & bmi_0_0!=.
replace bmi_cat = 4 if bmi_0_0 >=30 & bmi_0_0!=.
lab def bmi_cat 1 "Underweight" 2 "Healthy weight" 3"Overweight" 4 "Obese", modify
lab val bmi_cat bmi_cat
lab var bmi_cat "Categories of BMI"

gen bmi_binary = 0 if bmi_0_0 <25 & bmi_0_0!=.
replace bmi_binary = 1 if bmi_0_0 >=25 & bmi_0_0!=.
lab def bmi_binary 0 "Healthy/underweight" 1 "Overweight/obese"
lab val bmi_binary bmi_binary
lab var bmi_binary "BMI healthy vs overweight"

gen former_smoke = 0 if current_smoke==0
replace former_smoke = 0 if current_smoke==1 & former_smoke==.
replace former_smoke=1 if current_smoke==2 & former_smoke==.
lab var former_smoke "Never/former vs current smoker"
lab def former_smoke 0 "Never/former smoker" 1 "Current smoker"
lab val former_smoke former_smoke


*****************************************************************
* Tabulations for testing/test positivity by BMI and smoking
*****************************************************************

* Open log file 
log using test_positivity_20210729.log, replace

*****************************************************************
* Not stratified by BMI
*****************************************************************

* Calculate the proportion of tests
tab test_phase1

* Calculate the proportion of positive tests compared to population controls
tab positive_test_pop_phase1

* Calculate the proportion of positive tests taken in the tested sample
tab positive_test_negative_phase1

*****************************************************************
* Stratified by BMI
*****************************************************************

*****************************************************************
* Stratified by categorical BMI (underweight/healthy/overweight/obese)

* Calculate the proportion of tests by strata of BMI
tab bmi_cat test_phase1, row chi

* Calculate the proportion of positive tests compared to population controls by strata of BMI
tab bmi_cat positive_test_pop_phase1, row chi

* Calculate the proportion of positive tests in the tested sample by strata of BMI
tab bmi_cat positive_test_negative_phase1, row chi

*****************************************************************
* Stratified by BMI and smoking
*****************************************************************

*****************************************************************
* Stratified by categorical BMI (underweight/healthy/overweight/obese)

* Calculate the proportion of tests by strata of BMI
tab bmi_cat test_phase1 if former_smoke==0, row chi
tab bmi_cat test_phase1 if former_smoke==1, row chi

* Calculate the proportion of positive tests compared to population controls by strata of BMI
tab bmi_cat positive_test_pop_phase1 if former_smoke==0, row chi
tab bmi_cat positive_test_pop_phase1 if former_smoke==1, row chi

* Calculate the proportion of positive tests in the tested sample by strata of BMI
tab bmi_cat positive_test_negative_phase1, row chi

*****************************************************************

** Tab tested by BMI and smoking categories
* Never smokers
tab bmi_cat test_phase1 if former_smoke==0, row chi
* Ever smokers
tab bmi_cat test_phase1 if former_smoke==1, row chi

*****************************************************************

** Tab test positive in tested sample by BMI and smoking categories
* Never smokers
tab bmi_cat positive_test_negative_phase1 if former_smoke==0, row chi
* Ever smokers
tab bmi_cat positive_test_negative_phase1 if former_smoke==1, row chi

*****************************************************************

** Tab test positive in whole population sample by BMI and smoking categories
* Never smokers
tab bmi_cat positive_test_pop_phase1 if former_smoke==0, row chi
* Ever smokers
tab bmi_cat positive_test_pop_phase1 if former_smoke==1, row chi

capture log close

*****************************************************************
*Poisson model for BMI*smoke interaction
*****************************************************************
*****************************************************************
log using BMI_smoke_poisson_20210811.log, replace
*****************************************************************

* No interaction
poisson test_phase1 former_smoke sd_bmi

* Interaction
poisson test_phase1 former_smoke##c.sd_bmi

capture log close
