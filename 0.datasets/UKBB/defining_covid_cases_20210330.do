/*

A.R. Carter // 24/03/2021
Defining COVID-19 infection and severity in UK Biobank
This uses COVID-19 testing and mortality data released on the 13/02/2021

Throughout this script and subsequent analyses, phase 1 variables refer to the pre-mass testing period (before 18th May 2020) and phase 2 variables refer to the post-mass testing period (18th May 2020 onwards)

At the time of writing, follow up was until the 1st february

*/


* Use the data containing variables which will be used as covariates for analysis
* This dataset has withdrawals removed, but no other exclusions have been applied. These are applied in this script
use "$resDir/data/COVIDITY/UKBB/analysis_variables_202103.dta", clear

* Change the working directory
cd "$resDir/data/COVIDITY/UKBB"

* Exclusions 
/* Remove for pregnancy */
drop if n_3140_0_0 ==1
drop if n_3140_0_0 ==2

** Exclude ppts if already died of non-covid causes until the 1st january 2020 - Anyone with a pre-pandemic death cannot by definition be a covid case, but anyone who dies during the pandemic can be a case. May want to revise the exclusion date
drop if date_of_death < date("20200101", "YMD")

* Restrict to English participants - cannot have a covid test if they are from Wales/Scotland/NI 

drop if n_54_0_0==11003 | n_54_0_0==11004 | n_54_0_0==11005 | n_54_0_0==11022 | n_54_0_0==11023

/*

		Total N = 421,037
		N Tests = 59,426
		N untested = 361,611
		N COVID-19 positive = 14,238 (PHE tests only)
		N COVID-19 negative = 45,188 (PHE tests only)
		N COVID-19 death = 590 (tested or suspected)

*/

						* Create COVID-19 definitions *
********************************************************************************
							* Reference variables *						
********************************************************************************

* Tested vs non-tested 
* Tested defined as having a COVID test in the PHE data 
* This uses the variable covid_test defined in the script cleaning_covid_data.do. Participants with a test are already set to 1, so anyone missing (because they were not in the PHE data) are set to 0
replace covid_test = 0 if covid_test==.
lab var covid_test "Non-tested vs covid tested (any setting)"

* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if they do not have their test data/results

********************************************************************************

* This is  a variable for those who have died either from COVID or other causes compared with those alive
* The variable covid_death is defined in the script cleaning_covid_data.do.This sets anyone with a non-covid death to 1 and a covid death to 2
* This set of code assigns anyone with no data (i.e., missing) on death to 0
gen any_death = 0 if covid_death==.
replace any_death = 1 if covid_death == 1
replace any_death = 2 if covid_death ==2
lab def any_death 0 "Alive" 1 "Non Covid death" 2 "Covid death", modify
lab val any_death any_death
lab var any_death "Alive vs Death in 2020 from covid or non-covid causes"

* Note that some of the "non-covid" deaths may still have received a positive covid test

********************************************************************************

* Variable combining suspected and tested covid deaths according to ICD codes used, not according to test data
* This replaces data in the covid_death variable derived from the script cleaning_covid_data.do. Here, we set any non-covid deaths to 0 (the same as those with no death) and covid deaths are set to 1
* Very few deaths are defined as suspected covid, there are also a number of covid deaths recorded as U07.1 indicating they were tested without test data, so all deaths will be treated equally

replace covid_death = 0 if covid_death==.
replace covid_death = 0 if covid_death==1 /* This line sets non-covid deaths to a controls, they are not excluded from analyses */
replace covid_death = 1 if covid_death==2
replace covid_death_test = 0 if covid_death_test==. & covid_death_suspect!=1
replace covid_death_suspect = 0 if covid_death_suspect==. & covid_death_test!=1

lab def covid_death 0 "Alive or non-covid death" 1 "Covid death", modify
lab val covid_death covid_death
lab val covid_death_test covid_death
lab val covid_death_suspect covid_death 
lab var covid_death "Alive  or non-covid death (all ppts.) vs covid death"

* Set deaths to pre/post mass testing
gen covid_death_phase1 = 1 if covid_death==1 &  date_of_death < date("20200518", "YMD")
gen covid_death_phase2 = 1 if covid_death==1 &  date_of_death >= date("20200518", "YMD")

********************************************************************************
						* Setting pre/post mass testing variables *
********************************************************************************

/*
This section of code creates 6 variables based on pre/post mass testing. Only cases are defined here. Controls and variable labels are assigned in the script below.

These variables are the main variables used to define the different case/control definitions in the script and are called on throughout
*/

* Create case definition variables that will be populated in the loops below
* variables with "test" in the name refer ot those based on test data only
* Variables with data in the name, refer to those based both on test data and mortality stats
capture drop test_phase1 test_phase2 positive_test_phase1 positive_test_phase2 negative_test_phase1 negative_test_phase2
gen test_phase1=.
gen test_phase2=.
gen positive_test_phase1=.
gen positive_test_phase2=.
gen negative_test_phase1=.
gen negative_test_phase2=.

capture drop data_phase1 data_phase2 positive_phase1 positive_phase2 negative_phase1 negative_phase2
gen data_phase1=.
gen data_phase2=.
gen positive_phase1=.
gen positive_phase2=.
gen negative_phase1=.
gen negative_phase2=.

* Loop to assign participants to have a covid test, and a positive covid test, in the pre/post mass testing periods
* Participants can have multiple covid tests and therefore, can have a (positive) test in both periods. However participants can only die once, so if they have a covid death in phase 1, they can't also have a covid death in phase 2
 
forval i = 1/54 {

	* COVID test pre mass testing
	replace test_phase1 = 1 if test_date_`i' < date("18052020", "DMY") & test_date_`i'!=. & test_phase1==.
	* COVID test or death pre mass testing
	replace data_phase1 = 1 if test_phase1==1 | covid_death_phase1==1 & data_phase1==.
	
	* Covid test post mass testing
	replace test_phase2 = 1 if test_date_`i' >= date("18052020", "DMY") & test_date_`i'!=. & test_phase2==.
	* Covid test or death post mass testing
	replace data_phase2 = 1 if test_phase2==1 & covid_death_phase1!=1 | covid_death_phase2==1 & covid_death_phase1!=1 & data_phase2==.
	
	* Covid positive pre mass testing
	replace positive_test_phase1 = 1 if test_date_`i' < date("18052020", "DMY") & test_date_`i'!=. & positive_test_phase1==. & covid_test_result==1
	* Covid positive or death pre mass testing
	replace positive_phase1 =1 if positive_test_phase1==1 | covid_death_phase1==1 & positive_phase1==.
	
	* Covid positive post mass testing
	replace positive_test_phase2 = 1 if test_date_`i' >= date("18052020", "DMY") & test_date_`i'!=. & positive_test_phase2==. & test_date_`i'!=. & covid_test_result==1
	* Covid positive or death post mass testing
	replace positive_phase2 = 1 if positive_test_phase2==1 & covid_death_phase1!=1 | covid_death_phase2==1 & covid_death_phase1!=1 & positive_phase2==.
	
	* Covid negative pre mass testing
	replace negative_test_phase1 = 1 if test_date_`i' < date("18052020", "DMY") & test_date_`i'!=. & negative_test_phase1==. & covid_test_result==0
	* Covid negative pre mass testing excluding deaths
	replace negative_phase1 = 1 if negative_test_phase1==1 & covid_death_phase1!=1 & negative_phase1==.
	
	* Covid negative post mass testing
	replace negative_test_phase2 = 1 if test_date_`i' >= date("18052020", "DMY") & test_date_`i'!=. & negative_test_phase2==. & test_date_`i'!=. & covid_test_result==0
	* Covid negative post mass testing excluding deaths
	replace negative_phase2 = 1 if negative_test_phase2==1 & covid_death_phase2!=1 & covid_death_phase1!=1 & negative_phase2==.
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

* Post mass testing
replace test_phase2 = 0 if test_phase2==.

lab var test_phase2 "Non-tested vs  Covid test after mass testing "
lab def test_phase2 0 "No test post mass testing'" 1 "Test during mass testing", modify
lab val test_phase2 test_phase2

********************************************************************************
* Variables for having test data for COVID stratified by testing period
* Defined based on date of first test or date of death

* Pre-mass testing
replace data_phase1 = 0 if data_phase1==. 

lab var data_phase1 "Non-tested vs Covid test/death before mass testing "
lab def data_phase1 0 "No test or death pre-mass testing" 1 "Tested/died pre-mass testing", modify
lab val data_phase1 data_phase1 

* Post mass testing
replace data_phase2 = 0 if data_phase2==. & covid_death_phase1!=1

lab var data_phase2 "Non-tested vs Covid test/death after mass testing "
lab def data_phase2 0 "No test or death post-mass testing" 1 "Tested/died post-mass testing", modify
lab val data_phase2 data_phase2 

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

* Post-mass testing
gen positive_test_nontested_phase2 = 1 if positive_test_phase2==1
replace positive_test_nontested_phase2 = 0 if test_phase2==0

lab def positive_test_nontested_phase2 0 "No test post mass testing" 1 "Covid positive confirmed via test post mass testing", modify
lab val positive_test_nontested_phase2 positive_test_nontested_phase2
lab var positive_test_nontested_phase2 "Non-tested vs Covid positive confirmed via test post mass testing"

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

* Post-mass testing
gen positive_nontested_phase2 = 1 if positive_phase2==1 
replace positive_nontested_phase2 = 0 if data_phase2==0 & covid_death_phase1!=1

lab def positive_nontested_phase2 0 "No test post mass testing" 1 "Covid positive test/death confirmed via test post mass testing", modify
lab val positive_nontested_phase2 positive_nontested_phase2
lab var positive_nontested_phase2 "Non-tested vs Covid positive confirmed via test post mass testing"

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

* Post-mass testing
gen negative_test_nontested_phase2 = 1 if negative_test_phase2==1
replace negative_test_nontested_phase2 = 0 if test_phase2==0

lab def negative_test_nontested_phase2 0 "No test post-mass testing" 1 "Covid negative confirmed via test post mass testing", modify
lab val negative_test_nontested_phase2 negative_test_nontested_phase2
lab var negative_test_nontested_phase2 "Non-tested vs Covid negative confirmed via test post mass testing"

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

* Post-mass testing
gen negative_nontested_phase2 = 1 if negative_phase2==1 
replace negative_nontested_phase2 = 0 if data_phase2==0 & covid_death_phase1!=1

lab def negative_nontested_phase2 0 "No test/death post mass testing" 1 "Covid negative confirmed via test post mass testing", modify
lab val negative_nontested_phase2 negative_nontested_phase2
lab var negative_nontested_phase2 "Non-tested vs Covid negative confirmed via test post mass testing"

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

* Post mass testing period
* Phase 1 test positive participants are excluded from this variable and set to missing
* This
gen positive_test_pop_phase2 = 1 if positive_test_phase2==1
replace positive_test_pop_phase2 = 0 if positive_test_phase2==. 

lab def positive_test_pop_phase2 0 "No test/test negative" 1 "Covid test positive", modify
lab val positive_test_pop_phase2 positive_test_pop_phase2
lab var positive_test_pop_phase2 "all ppts. (inc -ive test) vs Covid positive confirmed via test in post-mass testing"

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

* Post mass testing period
* Phase 1 test positive participants are excluded from this variable and set to missing
gen positive_pop_phase2 = 1 if positive_phase2==1 
replace positive_pop_phase2 = 0 if positive_phase2==. & covid_death_phase1!=1

lab def positive_pop_phase2 0 "No test/test negative" 1 "Covid test/death positive", modify
lab val positive_pop_phase2 positive_pop_phase2
lab var positive_pop_phase2 "all ppts. (inc -ive test) vs Covid positive test/death in post-mass testing"

********************************************************************************

********************************************************************************
							* Case vs Controls (tested) *
********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case
* Note that an individual can be a control in phase 1, if their first COVID-19 test was carried out pre-mass testing, but be a case in phase 2 if their first *positive* COVID-19 test was carried out post mass-testing (N=249)

* Pre-mass testing
gen positive_test_negative_phase1 = 1 if positive_test_phase1==1
replace positive_test_negative_phase1 = 0 if negative_test_phase1==1

lab def positive_test_negative_phase1 0 "Test negative" 1 "Test positive", modify
lab val positive_test_negative_phase1 positive_test_negative_phase1
lab var positive_test_negative_phase1 "Test negative (ref) vs test positive pre-mass testing"

* Post-mass testing
gen positive_test_negative_phase2 = 1 if positive_test_phase2==1
replace positive_test_negative_phase2 = 0 if negative_test_phase2==1

lab def positive_test_negative_phase2 0 "Test negative" 1 "Test positive", modify
lab val positive_test_negative_phase2 positive_test_negative_phase1
lab var positive_test_negative_phase2 "Test negative (ref) vs test positive post-mass testing"

********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test
* This variables does not include COVID-19 deaths without a COVID-19 test as a case
* Note that an individual can be a control in phase 1, if their first COVID-19 test was carried out pre-mass testing, but be a case in phase 2 if their first *positive* COVID-19 test was carried out post mass-testing (N=249)

* Pre-mass testing
gen positive_negative_phase1 = 1 if positive_phase1==1 
replace positive_negative_phase1 = 0 if negative_phase1==1


lab def positive_negative_phase1 0 "Test negative" 1 "Test/death positive", modify
lab val positive_negative_phase1 positive_negative_phase1
lab var positive_negative_phase1 "Test negative (ref) vs test/death positive pre-mass testing"

* Post-mass testing
gen positive_negative_phase2 = 1 if positive_phase2==1 
replace positive_negative_phase2 = 0 if negative_phase2==1 & covid_death_phase1!=1
lab def positive_negative_phase2 0 "Test negative" 1 "Test/death positive", modify
lab val positive_negative_phase2 positive_negative_phase2
lab var positive_negative_phase2 "Test negative (ref) vs test/death positive post-mass testing"
********************************************************************************
********************************************************************************

						* COVID-19 DEATH/SEVERITY *

********************************************************************************
						* Death vs non-severe covid (+ test) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* non-severe covid = positive test taken that didn't result in death

* Pre mass testing
gen death_nonsevere_phase1 = 1 if covid_death_phase1==1
replace death_nonsevere_phase1 = 0 if positive_phase1==1 & death_nonsevere_phase1==.
lab var death_nonsevere_phase1 "Non-severe covid  vs covid death pre-mass testing"
lab def death_nonsevere_phase1 0 "Covid" 1 "Covid death", modify
lab val death_nonsevere_phase1 death_nonsevere_phase1

* Post mass testing
gen death_nonsevere_phase2 = 1 if covid_death_phase2==1
replace death_nonsevere_phase2 = 0 if positive_phase2==1 & death_nonsevere_phase1!=1 & death_nonsevere_phase2==.
lab var death_nonsevere_phase2 "Non-severe covid  vs covid death post-mass testing"
lab def death_nonsevere_phase2 0 "Covid" 1 "Covid death", modify
lab val death_nonsevere_phase2 death_nonsevere_phase2

********************************************************************************
							* Death vs tested (+/-) *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = COVID-19 test, either positive or negative

* Pre mass testing
gen death_tested_phase1 = 1 if covid_death_phase1==1
replace death_tested_phase1 = 0 if death_tested_phase1==. & data_phase1==1
lab var death_tested_phase1 "Tested (+ive and -ive) vs covid death pre mass testing"
lab def death_tested_phase1 0 "Tested" 1 "Covid death"
lab val death_tested_phase1 death_tested_phase1

* Post mass testing
gen death_tested_phase2 = 1 if covid_death_phase2==1
replace death_tested_phase2 = 0 if death_tested_phase2==. & data_phase2==1 & death_tested_phase1!=1
lab var death_tested_phase2 "Tested (+ive and -ive) vs covid death post mass testing"
lab def death_tested_phase2 0 "Tested" 1 "Covid death"
lab val death_tested_phase2 death_tested_phase2

********************************************************************************
							* Death vs test negative *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = negative test taken that didn't result in death

* Pre mass testing
gen death_negative_phase1 = 1 if covid_death_phase1==1
replace death_negative_phase1 = 0 if positive_negative_phase1==0 & death_negative_phase1==.
lab var death_negative_phase1 "Test negative  vs covid death pre-mass testing"
lab def death_negative_phase1 0 "Covid test negative" 1 "Covid death", modify
lab val death_negative_phase1 death_negative_phase1

* Post mass testing
gen death_negative_phase2 = 1 if covid_death_phase2==1
replace death_negative_phase2 = 0 if positive_negative_phase2==0 & death_negative_phase1!=1 & death_negative_phase2==.
lab var death_negative_phase2 "Test negative  vs covid death post-mass testing"
lab def death_negative_phase2 0 "Covid test negative" 1 "Covid death", modify
lab val death_negative_phase2 death_negative_phase2

********************************************************************************
********************************************************************************
							* Death vs population *
********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Control = all participants without a covid death

* Pre mass testing
gen death_population_phase1 = 1 if covid_death_phase1==1
replace death_population_phase1 = 0 if death_population_phase1==.

lab var death_population_phase1 "No covid death  vs covid death pre-mass testing"
lab def death_population_phase1 0 "No covid death" 1 "Covid death", modify
lab val death_population_phase1 death_population_phase1

* Post mass testing
gen death_population_phase2 = 1 if covid_death_phase2==1
replace death_population_phase2 = 0 if death_population_phase1!=1 & death_population_phase2==.

lab var death_population_phase2 "No covid death  vs covid death post-mass testing"
lab def death_population_phase2 0 "No covid death" 1 "Covid death", modify
lab val death_population_phase2 death_population_phase2


********************************************************************************
									* SAVE *
********************************************************************************
foreach var of varlist test_phase1 test_phase2 data_phase1 data_phase2 positive_test_nontested_phase1 positive_test_nontested_phase2 positive_nontested_phase1 positive_nontested_phase2 negative_test_nontested_phase1 negative_test_nontested_phase2 negative_nontested_phase1 negative_nontested_phase2 positive_test_pop_phase1 positive_test_pop_phase2 positive_pop_phase1 positive_pop_phase2 positive_test_negative_phase1 positive_test_negative_phase2 positive_negative_phase1 positive_negative_phase2 death_nonsevere_phase1 death_nonsevere_phase2 death_tested_phase1 death_tested_phase2 death_negative_phase1 death_negative_phase2 death_population_phase1 death_population_phase2 {
	
	tab `var'
	
}

save "covidity_data_20210330.dta", replace

