/*
Alice R. Carter - 26/3/21
Cleaning COVID-19 test/death data to merge with UK Biobank baseline data
*/

* Test data

* Import test data
import delimited "$dataDir/2021-02-24/data/covid19_result-20210213.txt", clear 

* Create string variable for test data from specdate variable
gen double test_date_ = date(specdate,"DMY")
format test_date %td

* Count the number of covid tests per person and maximum taken
* The count variable will be used to reshape the data to provide one set of observations for every test taken
by eid, sort: gen count_test=_n
by eid: egen n_tests_ = max(count_test)
lab var n_tests "Number of covid tests taken"

* Rename each variable to have a _ suffix to make reshaped data more clear
foreach var in specdate spectype laboratory origin result acute hosaq reqorg {
	
	rename `var' `var'_
}

* Reshape the data from long to wide format providing one set of observations for every test taken
reshape wide n_tests_ test_date_ specdate_ spectype_ laboratory_ origin_ result_ acute_ hosaq_ reqorg_, i(eid) j(count_test)

* Drop repeated versions of n_tests_
* Keep just the first set, as all variables are the same for all individuals and rename variable 
forvalues i = 2/54 {
drop n_tests_`i'
}
rename n_tests_1 number_covid_tests

* Loop through all results to identify which tests are positive and create covid positive vs covid negative variable
gen covid_test_result =0
forvalues i = 1/54 {
foreach var in result_`i' {
	
	replace covid_test_result = 1 if `var'==1
}
}
lab var covid_test_result "Any COVID-19 test positive"
lab def covid_test_result 0 "COVID-19 negative" 1 "COVID-19 positive"
lab val covid_test_result covid_test_result

* Loop through origin variables to identify whether the test was carried out in hospital or in the community
gen inpatient_covid =  0
forvalues i = 1/54 {
foreach var in origin_`i' {
	
	replace inpatient_covid = 1 if `var'==1
}
}
lab var inpatient_covid "Was any covid test carried out in hospital"
lab def inpatient_covid 0 "No" 1  "Yes"
lab val inpatient_covid inpatient_covid

/*
* Confirm these variables are definitely no longer needed before deleting the code

egen first_test = min(test_date), by(eid)
format first_test %td
lab var first_test "Date of first covid test"

bysort eid: gen positive_test_date = test_date if result==1
format positive_test_date %td
lab var positive_test_date "Date of positive covid test"

egen first_positive_test = min(positive_test_date), by(eid)
replace first_positive_test = first_test if first_positive_test==.
format first_positive_test %td
lab var first_positive_test "Date of first (positive) covid test"
*/

gen repeat_covid_test = 0 if number_covid_tests>1
replace repeat_covid_test = 1 if repeat_covid_test==.
lab var repeat_covid_test "More than one covid test for participant"
lab def repeat 0 "One test only" 1 "More than one covid test"
lab val repeat_covid_test repeat

* Create a variable for having a covid test. All participants in this dataset are set to 1. This could also be made in the main dataset based on the _merge variable
gen covid_test = 1
lab var covid_test "Covid test taken"
lab def covid_test 0 "No covid test" 1 "Covid test taken"
lab val covid_test covid_test

save "$resDir/data/COVIDITY/UKBB/covid_test_202103.dta", replace

*Death data
*import delimited 
import delimited "$dataDir/2021-02-24/data/death_cause-20210213.txt", clear

gen covid_death_test_x=1 if cause_icd10=="U071"
gen covid_death_suspect_x=1 if cause_icd10=="U072"

bysort eid: egen covid_death_test = max(covid_death_test_x)
replace covid_death_test = 0 if covid_death_test==.

bysort eid: egen covid_death_suspect = max(covid_death_suspect_x)
replace covid_death_suspect = 0 if covid_death_suspect==. & covid_death_test==0

gen covid_death = 2 if covid_death_test==1 
replace covid_death = 2 if covid_death_suspect==1 & covid_death==.
replace covid_death = 1 if covid_death==.

lab def covid_death 0 "Alive" 1 "Non-covid death" 2"Any covid death"
lab val covid_death covid_death

lab val covid_death_test casecontrol
lab val covid_death_suspect casecontrol

lab var covid_death "Suspected or confirmed covid death"
lab var covid_death_test "Covid death confirmed with test"
lab var covid_death_suspect "Suspected covid death (no test)"

save "$resDir/data/COVIDITY/UKBB/covid_death_202102.dta", replace

** Adding date of death

import delimited "$dataDir/2021-02-24/data/death-20210213.txt", clear

merge m:m eid using "$resDir/data/COVIDITY/UKBB/covid_death_202102.dta"

keep if _merge==3

gen double date = date(date_of_death,"DMY")
format date %td

drop date_of_death
rename date date_of_death

duplicates tag eid, gen(duplicate)
duplicates drop eid, force

save "$resDir/data/COVIDITY/UKBB/covid_death_dates_202102.dta", replace
