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


* generate covid variables for phase 1 period
do "$scriptDir/0.datasets/UKBB/generate_data_for_phase.do" "phase1" "20200101" "20200518"

* generate covid variables for phase2 period
do "$scriptDir/0.datasets/UKBB/generate_data_for_phase.do" "phase2" "20200518" "20210331"

local phase1_vars test_phase1 data_phase1 positive_test_nontested_phase1 positive_nontested_phase1 negative_test_nontested_phase1 negative_nontested_phase1 positive_test_pop_phase1 positive_pop_phase1 positive_test_negative_phase1 positive_negative_phase1 death_nonsevere_phase1 death_tested_phase1 death_negative_phase1 death_population_phase1
local phase2_vars test_phase2 data_phase2 positive_test_nontested_phase2 positive_nontested_phase2 negative_test_nontested_phase2 negative_nontested_phase2 positive_test_pop_phase2 positive_pop_phase2 positive_test_negative_phase2 positive_negative_phase2 death_nonsevere_phase2 death_tested_phase2 death_negative_phase2 death_population_phase2

foreach var of varlist `phase1_vars' `phase2_vars' {
	tab `var'
}

save "covidity_data_202104.dta", replace
