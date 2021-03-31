/*

A.R. Carter // 24/03/2021
Defining COVID-19 infection and severity in UK Biobank
This uses COVID-19 testing and mortality data released on the 13/02/2021

*/



* get options from args

local phase = "`1'"
local startDate = "`2'"
local endDate = "`3'"



/*
We create 6 variables based on pre/post mass testing. Only cases are defined here. Controls and variable labels are assigned in the script below.

These variables are the main variables used to define the different case/control definitions in the script and are called on throughout
*/

* Create case definition variables that will be populated in the loops below
* variables with "test" in the name refer to those based on test data only
* Variables with data in the name, refer to those based both on test data and mortality stats
capture drop test_phase1 positive_test_phase1 negative_test_phase1
gen test_phase1=.
gen positive_test_phase1=.
gen negative_test_phase1=.

capture drop data_phase1 positive_phase1 negative_phase1
gen data_phase1=.
gen positive_phase1=.
gen negative_phase1=.

* Loop to assign participants to have a covid test, and a positive covid test, in the pre/post mass testing periods
* Participants can have multiple covid tests and therefore, can have a (positive) test in both periods. However participants can only die once, so if they have a covid death in phase 1, they can't also have a covid death in phase 2
 
forval i = 1/54 {

	* COVID test in phase
	replace test_phase1 = 1 if test_date_`i' >= date("`startDate'", "DMY") & test_date_`i' < date("`endDate'", "DMY") & test_date_`i'!=. & test_phase1==.

	* COVID test or covid death in phase
	replace data_phase1 = 1 if test_phase1==1 | covid_death_phase1==1 & data_phase1==.
	
	* Covid positive in phase
	replace positive_test_phase1 = 1 if test_date_`i' >= date("`startDate'", "DMY") & test_date_`i' < date("`endDate'", "DMY") & test_date_`i'!=. & positive_test_phase1==. & covid_test_result==1

	* Covid positive or covid death in phase
	replace positive_phase1 =1 if positive_test_phase1==1 | covid_death_phase1==1 & positive_phase1==.
	
	* Covid negative in phase
	replace negative_test_phase1 = 1 if test_date_`i' >= date("`startDate'", "DMY") & test_date_`i' < date("`endDate'", "DMY") & test_date_`i'!=. & negative_test_phase1==. & covid_test_result==0

	* Covid negative excluding deaths in phase
	replace negative_phase1 = 1 if negative_test_phase1==1 & covid_death_phase1!=1 & negative_phase1==.
	
}

********************************************************************************
							* Setting control definitions *
********************************************************************************
	
							* COVID-19 testing *
********************************************************************************
********************************************************************************
							* Assessed vs non-assessed *
********************************************************************************

/*
Setting the control (not assessed) group to zero, based on the case defintion defined in the loop above
*/

* Variables for having test data for COVID stratified by testing period
* Only defined based on tests, not on deaths 

* Pre-mass testing
replace test_phase1 = 0 if test_phase1==. 

lab var test_phase1 "Non-tested vs Covid test before mass testing "
lab def test_phase1 0 "No test pre-mass testing" 1 "Test pre-mass testing", modify
lab val test_phase1 test_phase1


********************************************************************************
* Variables for having test data for COVID stratified by testing period
* Defined based on date of first test or date of death

* Pre-mass testing
replace data_phase1 = 0 if data_phase1==. 

lab var data_phase1 "Non-tested vs Covid test/death before mass testing "
lab def data_phase1 0 "No test or death pre-mass testing" 1 "Tested/died pre-mass testing", modify
lab val data_phase1 data_phase1 


					* COVID-19 INFECTION/SUSCPETIBILITY *
********************************************************************************

********************************************************************************
		* Case (COVID-19 +) vs non-assessed (assumed COVID-19 negative) *
********************************************************************************

/*
Setting the control (not assessed) group to zero, based on the case defintion defined in the loop above
*/

* Variable for Covid positive participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a positive PHE COVID-19 test (not considering deaths) as defined by positive_test_phase* in the loop above
* Control = anyone without a COVID-19 test in the relevant test period defined as anyone with no covid test in test_phase* defined previously

* Pre-mass testing
gen positive_test_nontested_phase1 = 1 if positive_test_phase1==1
replace positive_test_nontested_phase1 = 0 if test_phase1==0

lab def positive_test_nontested_phase1 0 "No test pre-mass testing" 1 "Covid positive confirmed via test pre-mass testing", modify
lab val positive_test_nontested_phase1 positive_test_nontested_phase1
lab var positive_test_nontested_phase1 "Non-tested vs Covid positive confirmed via test pre-mass testing"


********************************************************************************

* Variable for Covid positive participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a positive PHE COVID-19 test or had a COVID-19 death as defined by positive_phase* in the loop above
* Control = anyone without a COVID-19 test/death in the relevant test period defined as anyone with no covid test in data_phase* defined previously

* Pre-mass testing
gen positive_nontested_phase1 = 1 if positive_phase1==1 
replace positive_nontested_phase1 = 0 if data_phase1==0

lab def positive_nontested_phase1 0 "No test pre-mass testing" 1 "Covid positive test/death pre-mass testing", modify
lab val positive_nontested_phase1 positive_nontested_phase1
lab var positive_nontested_phase1 "Non-tested vs Covid positive test/death pre-mass testing"


********************************************************************************
	* Control (COVID-19 negative) vs non-assessed (assumed COVID-19 negative) *
********************************************************************************

* Variable for Covid negative participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a negative PHE COVID-19 test as defined by negative_test_phase* in the loop above
* Control = anyone without a COVID-19 test in the relevant test period (not considering deaths) defined as anyone with no covid test in test_phase* defined previously

* Pre-mass testing
gen negative_test_nontested_phase1 = 1 if negative_test_phase1==1
replace negative_test_nontested_phase1 = 0 if test_phase1==0

lab def negative_test_nontested_phase1 0 "No test pre-mass testing" 1 "Covid negative confirmed via test pre-mass testing", modify
lab val negative_test_nontested_phase1 negative_test_nontested_phase1
lab var negative_test_nontested_phase1 "Non-tested vs Covid negative confirmed via test pre-mass testing"

********************************************************************************

* Variable for Covid negative participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a positive PHE COVID-19 test or had a COVID-19 death as defined by negative_test_phase* in the loop above
* Control = anyone without a COVID-19 test/death in the relevant test period
* COVID-19 test positive participants, or those with a COVID-19 death are set to missing

* Pre-mass testing
gen negative_nontested_phase1 = 1 if negative_phase1==1 
replace negative_nontested_phase1 = 0 if data_phase1==0

lab def negative_nontested_phase1 0 "No test/death pre-mass testing" 1 "Covid negative pre-mass testing", modify
lab val negative_nontested_phase1 negative_nontested_phase1
lab var negative_nontested_phase1 "Non-tested vs Covid negative pre-mass testing"


********************************************************************************
					* Case (COVID-19 +) vs Controls (all population) *
********************************************************************************

* Variable for covid positive vs covid negative based on test data
* This is defined based on having a positive covid test, not accounting for deaths
* COVID negative is defined as no test or test negative
* This variable is based on the *date of first positive test* for stratifying on time

* Pre mass testing period
gen positive_test_pop_phase1 = 1 if positive_test_phase1==1
replace positive_test_pop_phase1 = 0 if positive_test_phase1==. 

lab def positive_test_pop_phase1 0 "No test/test negative" 1 "Covid test positive", modify
lab val positive_test_pop_phase1 positive_test_pop_phase1
lab var positive_test_pop_phase1 "all ppts. (inc -ive test) vs Covid positive confirmed via test in pre-mass testing"


* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if we do not have their test data/results

********************************************************************************

* Variable for covid positive vs covid negative based on test and mortality data
* This is defined based on having a positive covid test, or a mortality record with U07.1 or U07.2 recorded
* COVID negative is defined as no test or test negative
* This variable is based on the date of first positive test, or date of death, for stratifying on time

* Pre mass testing period
gen positive_pop_phase1 = 1 if positive_phase1==1 
replace positive_pop_phase1 = 0 if positive_phase1==.

lab def positive_pop_phase1 0 "No test/test negative" 1 "Covid test/death positive", modify
lab val positive_pop_phase1 positive_pop_phase1
lab var positive_pop_phase1 "all ppts. (inc -ive test) vs Covid positive test/death pre-mass testing"


********************************************************************************

********************************************************************************
							* Case vs Controls (tested) *
********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case
* Note that an individual can be a control in phase 1, if their first COVID-19 test was carried out pre-mass testing, but be a case in phase 2 if their first *positive* COVID-19 test was carried out post mass-testing (N=249)

gen positive_test_negative_phase1 = 1 if positive_test_phase1==1
replace positive_test_negative_phase1 = 0 if negative_test_phase1==1

lab def positive_test_negative_phase1 0 "Test negative" 1 "Test positive", modify
lab val positive_test_negative_phase1 positive_test_negative_phase1
lab var positive_test_negative_phase1 "Test negative (ref) vs test positive pre-mass testing"

********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case
* Note that an individual can be a control in phase 1, if their first COVID-19 test was carried out pre-mass testing, but be a case in phase 2 if their first *positive* COVID-19 test was carried out post mass-testing (N=249)

gen positive_negative_phase1 = 1 if positive_phase1==1 
replace positive_negative_phase1 = 0 if negative_phase1==1


lab def positive_negative_phase1 0 "Test negative" 1 "Test/death positive", modify
lab val positive_negative_phase1 positive_negative_phase1
lab var positive_negative_phase1 "Test negative (ref) vs test/death positive pre-mass testing"

********************************************************************************
********************************************************************************

						* COVID-19 DEATH/SEVERITY *

********************************************************************************
						* Death vs non-severe covid (+ test) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* non-severe covid = positive test taken that didn't result in death

gen death_nonsevere_phase1 = 1 if covid_death_phase1==1
replace death_nonsevere_phase1 = 0 if positive_phase1==1 & death_nonsevere_phase1==.
lab var death_nonsevere_phase1 "Non-severe covid  vs covid death pre-mass testing"
lab def death_nonsevere_phase1 0 "Covid" 1 "Covid death", modify
lab val death_nonsevere_phase1 death_nonsevere_phase1


********************************************************************************
							* Death vs tested (+/-) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = COVID-19 test, either positive or negative

gen death_tested_phase1 = 1 if covid_death_phase1==1
replace death_tested_phase1 = 0 if death_tested_phase1==. & data_phase1==1
lab var death_tested_phase1 "Tested (+ive and -ive) vs covid death pre mass testing"
lab def death_tested_phase1 0 "Tested" 1 "Covid death"
lab val death_tested_phase1 death_tested_phase1


********************************************************************************
							* Death vs test negative *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = negative test taken that didn't result in death

gen death_negative_phase1 = 1 if covid_death_phase1==1
replace death_negative_phase1 = 0 if positive_negative_phase1==0 & death_negative_phase1==.
lab var death_negative_phase1 "Test negative  vs covid death pre-mass testing"
lab def death_negative_phase1 0 "Covid test negative" 1 "Covid death", modify
lab val death_negative_phase1 death_negative_phase1


********************************************************************************
********************************************************************************
							* Death vs population *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = all participants without a covid death

gen death_population_phase1 = 1 if covid_death_phase1==1
replace death_population_phase1 = 0 if death_population_phase1==.

lab var death_population_phase1 "No covid death  vs covid death pre-mass testing"
lab def death_population_phase1 0 "No covid death" 1 "Covid death", modify
lab val death_population_phase1 death_population_phase1


********************************************************************************
									* SAVE *
********************************************************************************
foreach var of varlist test_phase1 test_phase2 data_phase1 data_phase2 positive_test_nontested_phase1 positive_test_nontested_phase2 positive_nontested_phase1 positive_nontested_phase2 negative_test_nontested_phase1 negative_test_nontested_phase2 negative_nontested_phase1 negative_nontested_phase2 positive_test_pop_phase1 positive_test_pop_phase2 positive_pop_phase1 positive_pop_phase2 positive_test_negative_phase1 positive_test_negative_phase2 positive_negative_phase1 positive_negative_phase2 death_nonsevere_phase1 death_nonsevere_phase2 death_tested_phase1 death_tested_phase2 death_negative_phase1 death_negative_phase2 death_population_phase1 death_population_phase2 {
	
	tab `var'
	
}

save "covidity_data_`phase'-20210330.dta", replace

