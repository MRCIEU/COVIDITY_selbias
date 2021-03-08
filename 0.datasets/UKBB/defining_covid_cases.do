/*

A.R. Carter // 04/03/2021
Defining COVID-19 infection and severity in UK Biobank
This uses COVID-19 testing and mortality data released on the 13/02/2021

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
						
					* COVID-19 INFECTION/SUSCPETIBILITY *
********************************************************************************

* Tested vs non-tested 
* Tested defined as having a COVID test in the PHE data 
replace covid_test = 0 if covid_test==.
lab var covid_test "Non-tested vs covid tested (any setting)"

* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if we do not have their test data/results

********************************************************************************

* This is  a variable for those who have died either from COVID or other causes compared with those alive
gen any_death = 0 if covid_death==.
replace any_death = 1 if covid_death == 1
replace any_death = 2 if covid_death ==2
lab def any_death 0 "Alive" 1 "Non Covid death" 2 "Covid death", modify
lab val any_death any_death
lab var any_death "Alive vs Death in 2020 from covid or non-covid causes"

* Note that some of the "non-covid" deaths may still have received a positive covid test

********************************************************************************

* Variable combining suspected and tested covid deaths according to ICD codes used, not according to test data
* Very few deaths are defined as suspected covid, there are also a number of covid deaths recorded as U07.1 indicating they were tested without test data, so all deaths will be treated equally

replace covid_death = 0 if covid_death==.
replace covid_death = 0 if covid_death==1 /* This line sets non-covid deaths to a control, they are not excluded from analyses */
replace covid_death = 1 if covid_death==2
replace covid_death_test = 0 if covid_death_test==. & covid_death_suspect!=1
replace covid_death_suspect = 0 if covid_death_suspect==. & covid_death_test!=1

lab def covid_death 0 "Alive" 1 "Covid death", modify
lab val covid_death covid_death
lab val covid_death_test covid_death
lab val covid_death_suspect covid_death 
lab var covid_death "Alive (all ppts.) vs covid death"

********************************************************************************

* Variable for covid positive vs covid negative
* This is defined based on either a positive test result OR a U07.1 death, but NOT a U07.2 death (I think the latter is more an oversight than deliberate decision)
* COVID negative is defined as no test or test negative

gen covid_positive = 1 if covid_test_result==1
replace covid_positive = 0 if covid_positive==. | covid_positive==0
lab def covid_positive 0 "No test/test negative" 1 "Covid - confirmed via test", modify
lab val covid_positive covid_positive
lab var covid_positive "all ppts. (inc -ive test) vs Covid positive confirmed via test"

* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if we do not have their test data/results

********************************************************************************

* Variable for Covid positive participants vs those who have not recieved a covid test, i.e., excluding test negative participants

gen covid_positive_nontested = 1 if covid_test_result==1
replace covid_positive_nontested = 0 if covid_test==0
lab def covid_positive_nontested 0 "No test" 1 "Covid positive - confirmed via test", modify
lab val covid_positive_nontested covid_positive_nontested
lab var covid_positive_nontested "non-tested only vs Covid positive confirmed via test"

* This variable does not include any mortality data. Therefore, participants could have a death of U07.1 recorded (COVID diagnosed via test) and be classed as "untested" if we do not have their test data/results

********************************************************************************

* Variable for Covid positive participants vs those who have tested negative, i.e., excluding participants who have never received a test

gen covid_positive_tested = 1 if covid_test_result==1
replace covid_positive_tested = 0 if covid_test==1 & covid_test_result==0
lab def covid_positive_tested 0 "Test negative" 1 "Covid positive - confirmed via test", modify
lab val covid_positive_tested covid_positive_tested
lab var covid_positive_tested "test negative (ref) vs test positive"

* Note that this variable is the same data as that in covid_test_result ut the variable label is more informative for comparing the case/control groups in analyses

********************************************************************************

* Variable for Covid negative participants vs all other participants, i.e., including test positive participants

gen covid_negative = 1 if covid_test_result==0
replace covid_negative = 0 if covid_negative==.
lab def covid_negative 0 "No test/test positive" 1 "Covid negative - confirmed via test" 
lab val covid_negative covid_negative
lab var covid_negative "all ppts. (inc +ive test) vs Covid negative confirmed via test"

********************************************************************************

* Variable for Covid negative participants vs those who have never received a test, i.e., excluding participants who have tested positive for covid

gen covid_negative_nontested = 1 if covid_test_result==0 
replace covid_negative_nontested = 0 if covid_test==0
lab def covid_negative_nontested 0 "No test" 1 "Covid negative- confirmed via test", modify
lab val covid_negative_nontested covid_negative_nontested
lab var covid_negative_nontested "non-tested vs Covid negative confirmed via test"

********************************************************************************

* Variable for Covid negative participants vs those who have tested positive, i.e., excluding participants who have never received a test

gen covid_negative_tested = 1 if covid_test_result==0
replace covid_negative_tested = 0 if covid_test==1 & covid_test_result==1
lab def covid_negative_tested 0 "Test positive" 1 "Covid negative - confirmed via test", modify
lab val covid_negative_tested covid_negative_tested
lab var covid_negative_tested "test positive (ref) vs test negative"

* Note that this variable is the inverse data as that in covid_test_result/covid_positive_tested 

********************************************************************************

						* COVID-19 DEATH/SEVERITY *

********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* non-severe covid = positive test taken that didn't result in death

gen death_vs_non_severe = 1 if covid_death==1
replace death_vs_non_severe = 0 if covid_positive==1 & death_vs_non_severe==.
lab var death_vs_non_severe "Non-severe covid  vs covid death"
lab def death_vs_non_severe 0 "Covid" 1 "Covid death", modify
lab val death_vs_non_severe death_vs_non_severe

* Note the sample size for total positive cases here is 14,346 which is larger than that in the test data (N=14,238)

********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Community tested = covid test taken in the community that was either positive or negative

gen death_vs_community = 1 if covid_death==1
replace death_vs_community = 0 if death_vs_community==. & inpatient_covid==0
lab var death_vs_community "Community covid tests (+ive and -ive) vs covid death"
lab def death_vs_community 0 "Community tested" 1 "Covid death"
lab val death_vs_community death_vs_community

********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Community tested = covid test taken in the community that was either positive or negative

gen death_vs_tested = 1 if covid_death==1
replace death_vs_tested = 0 if death_vs_tested==. & covid_test==1
lab var death_vs_tested "Tested (+ive and -ive) vs covid death"
lab def death_vs_tested 0 "Tested" 1 "Covid death"
lab val death_vs_tested death_vs_tested

* Note the sample size here is larger than the number of covid tests reported, indicating a number of covid deaths do not have test data

********************************************************************************

* Define severe covid through deaths 
* Death = any covid death, either confirmed or suspected
* Case = all remaining participants including test positive, test negative and untested participants

gen death_vs_all = 1 if covid_death==1
replace death_vs_all = 0 if death_vs_all==.
lab var death_vs_all "All ppts vs severe covid"
lab def death_vs_all 0 "All ppts (inc test +ive)" 1 "Covid death"
lab val death_vs_all death_vs_all

********************************************************************************

								* TIME *

********************************************************************************

* Simple variable to stratify by time based on before and after mass testing
* Mass testing set to begin on 28th May 2020 defined (see excel file from RH)

gen mass_test = 0 if first_test < date("20200528", "YMD") & first_test!=.
replace mass_test = 1 if first_test >= date("20200528", "YMD") & first_test!=. & mass_test==.
lab var mass_test "First Covid test before or after mass testing in tested ppts."
lab def mass_test 0 "Pre mass testing" 1 "Post mass testing", modify
lab val mass_test mass_test

save "covidity_data_20210304.dta", replace

