/*

A.R. Carter // 24/03/2021
Defining COVID-19 infection and severity in UK Biobank
This uses COVID-19 testing and mortality data released on the 13/02/2021

*/

* get options from args

local phase = "`1'"
local startDate = "`2'"
local endDate = "`3'"

di "`phase'"
di "`startDate'"
di "`endDate'"

* Exclude participants who died before the start of the phase, either from COVID-19 or non-COVID-19 causes
*drop if date_of_death!=. & date_of_death < date("`startDate'", "YMD")

/*
We create 6 variables based on COVID-19 testing and result. Only cases are defined here. Controls and variable labels are assigned in the script below.

These variables are the main variables used to define the different case/control definitions in the script and are called on throughout
*/

* Create case definition variables that will be populated in the loops below
* variables with "test" in the name refer to those based on test data only
* Variables with data in the name, refer to those based both on test data and mortality stats
capture drop test_`phase' positive_test_`phase' negative_test_`phase'
gen test_`phase'=.
gen positive_test_`phase'=.
gen negative_test_`phase'=.

capture drop data_`phase' positive_`phase' negative_`phase'
gen data_`phase'=.
gen positive_`phase'=.
gen negative_`phase'=.

* Loop to assign participants to have a covid test, and a positive covid test, in the pre/post mass testing periods
* Participants can have multiple covid tests and therefore, can have a (positive) test in both periods. However participants can only die once, so if they have a covid death in phase 1, they can't also have a covid death in phase 2
 
*  ssc install findname

findname test_date_*
local testdatevars = "`r(varlist)'"
foreach thistestdate in `testdatevars' { 

	di "`thistestdate'"

	* COVID test in phase
	replace test_`phase' = 1 if `thistestdate' >= date("`startDate'", "YMD") & `thistestdate' < date("`endDate'", "YMD") & `thistestdate'!=. & test_`phase'==.

	* COVID test or covid death in phase
	replace data_`phase' = 1 if (test_`phase'==1 | covid_death_`phase'==1) & data_`phase'==.

	* Covid positive in phase
	* get respective test result
	local i = substr( "`thistestdate'", 11,.)	
	di `i'
	* Set positive cases to 1
	replace positive_test_`phase' = 1 if `thistestdate' >= date("`startDate'", "YMD") & `thistestdate' < date("`endDate'", "YMD") & `thistestdate'!=. & positive_test_`phase'==. & result_`i'==1

	* Covid positive or covid death in phase
	replace positive_`phase' =1 if (positive_test_`phase'==1 | covid_death_`phase'==1) & positive_`phase'==.
	
	* Covid negative in phase
	replace negative_test_`phase' = 1 if `thistestdate' >= date("`startDate'", "YMD") & `thistestdate' < date("`endDate'", "YMD") & `thistestdate'!=. & negative_test_`phase'==. & result_`i'==0 

	* Set ppts to missing if they have a positive COVID-19 test (as well as negative) so they are only counted once per phase
	replace negative_test_`phase'=. if positive_test_`phase'==1

	* Covid negative excluding deaths in phase
	replace negative_`phase' = 1 if negative_test_`phase'==1 & covid_death_`phase'!=1 & negative_`phase'==.
	
	* Set ppts to missing if they have a positive COVID-19 test (as well as negative) so they are only counted once per phase
	replace negative_`phase'=. if positive_`phase'==1	
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

replace test_`phase' = 0 if test_`phase'==. 

lab var test_`phase' "Non-tested vs Covid test before `phase'  "
lab def test_`phase' 0 "No test `phase' " 1 "Test `phase'", modify
lab val test_`phase' test_`phase'


********************************************************************************
* Variables for having test data for COVID stratified by  period
* Defined based on date of first test or date of death

replace data_`phase' = 0 if data_`phase'==. 

lab var data_`phase' "Non-tested vs Covid test/death before `phase' "
lab def data_`phase' 0 "No test or death `phase'" 1 "Tested/died `phase'", modify
lab val data_`phase' data_`phase' 


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

capture drop positive_test_nontested_`phase'
gen positive_test_nontested_`phase' = 1 if positive_test_`phase'==1
replace positive_test_nontested_`phase' = 0 if test_`phase'==0

lab def positive_test_nontested_`phase' 0 "No test `phase'" 1 "Covid positive confirmed via test `phase'", modify
lab val positive_test_nontested_`phase' positive_test_nontested_`phase'
lab var positive_test_nontested_`phase' "Non-tested vs Covid positive confirmed via test `phase'"


********************************************************************************

* Variable for Covid positive participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a positive PHE COVID-19 test or had a COVID-19 death as defined by positive_phase* in the loop above
* Control = anyone without a COVID-19 test/death in the relevant test period defined as anyone with no covid test in data_phase* defined previously

capture drop positive_nontested_`phase'
gen positive_nontested_`phase' = 1 if positive_`phase'==1 
replace positive_nontested_`phase' = 0 if data_`phase'==0

lab def positive_nontested_`phase' 0 "No test " 1 "Covid positive test/death `phase'", modify
lab val positive_nontested_`phase' positive_nontested_`phase'
lab var positive_nontested_`phase' "Non-tested vs Covid positive test/death `phase'"


********************************************************************************
	* Control (COVID-19 negative) vs non-assessed (assumed COVID-19 negative) *
********************************************************************************

* Variable for Covid negative participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a negative PHE COVID-19 test as defined by negative_test_phase* in the loop above
* Control = anyone without a COVID-19 test in the relevant test period (not considering deaths) defined as anyone with no covid test in test_phase* defined previously

capture drop negative_test_nontested_`phase'
gen negative_test_nontested_`phase' = 1 if negative_test_`phase'==1
replace negative_test_nontested_`phase' = 0 if test_`phase'==0

lab def negative_test_nontested_`phase' 0 "No test `phase'" 1 "Covid negative confirmed via test `phase'", modify
lab val negative_test_nontested_`phase' negative_test_nontested_`phase'
lab var negative_test_nontested_`phase' "Non-tested vs Covid negative confirmed via test `phase'"

********************************************************************************

* Variable for Covid negative participants vs those who have not recieved a covid test, i.e., excluding test negative participants
* Case = anyone with a positive PHE COVID-19 test or had a COVID-19 death as defined by negative_test_phase* in the loop above
* Control = anyone without a COVID-19 test/death in the relevant test period
* COVID-19 test positive participants, or those with a COVID-19 death are set to missing

capture drop negative_nontested_`phase'
gen negative_nontested_`phase' = 1 if negative_`phase'==1 
replace negative_nontested_`phase' = 0 if data_`phase'==0

lab def negative_nontested_`phase' 0 "No test/death `phase'" 1 "Covid negative `phase'", modify
lab val negative_nontested_`phase' negative_nontested_`phase'
lab var negative_nontested_`phase' "Non-tested vs Covid negative `phase'"


********************************************************************************
					* Case (COVID-19 +) vs Controls (all population) *
********************************************************************************

* Variable for covid positive vs covid negative based on test data
* This is defined based on having a positive covid test, not accounting for deaths
* COVID negative is defined as no test or test negative
* This variable is based on the *date of first positive test* for stratifying on time

capture drop positive_test_pop_`phase'
gen positive_test_pop_`phase' = 1 if positive_test_`phase'==1
replace positive_test_pop_`phase' = 0 if positive_test_`phase'==. 

lab def positive_test_pop_`phase' 0 "No test/test negative" 1 "Covid test positive", modify
lab val positive_test_pop_`phase' positive_test_pop_`phase'
lab var positive_test_pop_`phase' "all ppts. (inc -ive test) vs Covid positive confirmed via test in `phase' "


* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if we do not have their test data/results

********************************************************************************

* Variable for covid positive vs covid negative based on test and mortality data
* This is defined based on having a positive covid test, or a mortality record with U07.1 or U07.2 recorded
* COVID negative is defined as no test or test negative
* This variable is based on the date of first positive test, or date of death, for stratifying on time

capture drop positive_pop_`phase'
gen positive_pop_`phase' = 1 if positive_`phase'==1 
replace positive_pop_`phase' = 0 if positive_`phase'==.

lab def positive_pop_`phase' 0 "No test/test negative" 1 "Covid test/death positive", modify
lab val positive_pop_`phase' positive_pop_`phase'
lab var positive_pop_`phase' "all ppts. (inc -ive test) vs Covid positive test/death `phase' "


********************************************************************************

********************************************************************************
							* Case vs Controls (tested) *
********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case

capture drop positive_test_negative_`phase'
gen positive_test_negative_`phase' = 1 if positive_test_`phase'==1
replace positive_test_negative_`phase' = 0 if negative_test_`phase'==1

lab def positive_test_negative_`phase' 0 "Test negative" 1 "Test positive", modify
lab val positive_test_negative_`phase' positive_test_negative_`phase'
lab var positive_test_negative_`phase' "Test negative (ref) vs test positive `phase'"

********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case

capture drop positive_negative_`phase'
gen positive_negative_`phase' = 1 if positive_`phase'==1 
replace positive_negative_`phase' = 0 if negative_`phase'==1

lab def positive_negative_`phase' 0 "Test negative" 1 "Test/death positive", modify
lab val positive_negative_`phase' positive_negative_`phase'
lab var positive_negative_`phase' "Test negative (ref) vs test/death positive `phase' "

********************************************************************************
********************************************************************************

						* COVID-19 DEATH/SEVERITY *

********************************************************************************
						* Death vs non-severe covid (+ test) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* non-severe covid = positive test taken that didn't result in death

capture drop death_nonsevere_`phase'
gen death_nonsevere_`phase' = 1 if covid_death_`phase'==1
replace death_nonsevere_`phase' = 0 if positive_`phase'==1 & death_nonsevere_`phase'==.

lab var death_nonsevere_`phase' "Non-severe covid  vs covid death `phase' "
lab def death_nonsevere_`phase' 0 "Covid" 1 "Covid death", modify
lab val death_nonsevere_`phase' death_nonsevere_`phase'


********************************************************************************
							* Death vs tested (+/-) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = COVID-19 test, either positive or negative

capture drop death_tested_`phase'
gen death_tested_`phase' = 1 if covid_death_`phase'==1
replace death_tested_`phase' = 0 if death_tested_`phase'==. & data_`phase'==1

lab var death_tested_`phase' "Tested (+ive and -ive) vs covid death `phase' "
lab def death_tested_`phase' 0 "Tested" 1 "Covid death", modify
lab val death_tested_`phase' death_tested_`phase'


********************************************************************************
							* Death vs test negative *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = negative test taken that didn't result in death

capture drop death_negative_`phase'
gen death_negative_`phase' = 1 if covid_death_`phase'==1
replace death_negative_`phase' = 0 if positive_negative_`phase'==0 & death_negative_`phase'==.

lab var death_negative_`phase' "Test negative  vs covid death `phase' "
lab def death_negative_`phase' 0 "Covid test negative" 1 "Covid death", modify
lab val death_negative_`phase' death_negative_`phase'


********************************************************************************
********************************************************************************
							* Death vs population *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = all participants without a covid death

capture drop death_population_`phase'
gen death_population_`phase' = 1 if covid_death_`phase'==1
replace death_population_`phase' = 0 if death_population_`phase'==.

lab var death_population_`phase' "No covid death  vs covid death `phase' "
lab def death_population_`phase' 0 "No covid death" 1 "Covid death", modify
lab val death_population_`phase' death_population_`phase'

********************************************************************************
	* Set deaths from Covid/non-covid causes before the phase to missing *
********************************************************************************

foreach var in test_`phase' data_`phase' positive_test_nontested_`phase' positive_nontested_`phase' negative_test_nontested_`phase' negative_nontested_`phase' positive_test_pop_`phase' positive_pop_`phase' positive_test_negative_`phase' positive_negative_`phase' death_nonsevere_`phase' death_tested_`phase' death_negative_`phase' death_population_`phase' {
	
	replace `var' = . if date_of_death!=. & date_of_death < date("`startDate'", "YMD")
}

********************************************************************************
									* SAVE *
********************************************************************************

 save "covidity_data_`phase'-20210407.dta", replace

