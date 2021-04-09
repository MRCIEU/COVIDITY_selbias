
* get options from args

local sampleDef = "`1'"
local caseDef = "`2'"

di "`sampleDef'"
di "`caseDef'"

********************************************************************************
* Association of covariates with BMI in all eligible UKBB participants in phase1 (defined by phase1_sample)

* Regression
regress sd_bmi i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi

* Storing and formatting results
matrix results = r(table)
matrix results = results[1..6,1..12]
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef'.xlsx, sheet(bmi_outcome_all) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
	putexcel A10="F stat" B10=`e(F)'
	putexcel B1="0b.eduyears - GCSE or less" C1="1.eduyears - AS/A level" D1="3.eduyears - NVQ/vocational quali" E1="4.eduyears - degree or higher" F1="0b.sex - female" G1="1.sex - male" H1="Age (SD)" I1="0b.current_smoke - never smoker" J1="1.current_smoke - former smoker" K1="2.current_smoke - current smoker" L1="TDI (SD)" 

********************************************************************************	
* Association of covariates with covid positive in tested sample
* Variable positive_test_negative_phase1 defines a case as a COVID-19 positive test and control as COVID-19 negative test

* Regression
logistic positive_`caseDef'_negative_phase1 i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi, coef

* Storing and formatting results
matrix results = r(table)
matrix results = results[1..6,1..12]
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef'.xlsx, sheet(covid_outcome_tested) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
	putexcel B2="0b.eduyears - GCSE or less" C2="1.eduyears - AS/A level" D2="3.eduyears - NVQ/vocational quali" E2="4.eduyears - degree or higher" F2="0b.sex - female" G2="1.sex - male" H2="Age (SD)" I2="0b.current_smoke - never smoker" J2="1.current_smoke - former smoker" K2="2.current_smoke - current smoker" L2="TDI (SD)" 
		
********************************************************************************		
* Association of BMI/covariates with receiving a covid test in the whole UKBB sample

* Regression
logistic `caseDef'_phase1 sd_bmi i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi, coef

* Storing and formatting results
matrix results = r(table)
matrix results = results[1..6,1..13]
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef'.xlsx, sheet(test_outcome_all) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
	putexcel B2="BMI (SD)" C2="0b.eduyears - GCSE or less" D2="1.eduyears - AS/A level" E2="3.eduyears - NVQ/vocational quali" F2="4.eduyears - degree or higher" G2="0b.sex - female" H2="1.sex - male" I2="Age (SD)" J2="0b.current_smoke - never smoker" K2="1.current_smoke - former smoker" L2="2.current_smoke - current smoker" M2="TDI (SD)" 
		
********************************************************************************	
* Prevalence and distribution estimates

putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
putexcel A1="Variable" B1="Variable type" C1="Level" D1="Prevalence/mean" E1="percent/SD"	

local x=1
foreach var in sex `caseDef'_phase1 positive_`caseDef'_pop_phase1 {

	local x=`x'+1	
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
	
	tab `var', matcell(numbers)
	
		local control = sum(numbers[1,1])
		local case = sum(numbers[2,1])
		
		local control_prop = (`control'/r(N)*100)
		local case_prop = (`case'/r(N)*100)
			
	local var_label: var label `var'

	
	putexcel A`x'="`var_label'" B`x'="Binary"  D`x'=`control' E`x'=`control_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Binary"  D`x'=`case' E`x'=`case_prop'

	}

local x=7
foreach var in current_smoke {
	
	local x=`x'+1	
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
	
	tab `var', matcell(numbers)
	
		local control = sum(numbers[1,1])
		local case_1 = sum(numbers[2,1])
		local case_2 = sum(numbers[3,1])
		
		local control_prop = (`control'/r(N)*100)
		local case_1_prop = (`case_1'/r(N)*100)
		local case_2_prop = (`case_2'/r(N)*100)
			
	local var_label: var label `var'

	
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`control' E`x'=`control_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`case_1' E`x'=`case_1_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`case_2' E`x'=`case_2_prop'
	
}

local x=10
foreach var in eduyears_quali {
	
	local x=`x'+1	
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
	
	tab `var', matcell(numbers)
	
		local level_0 = sum(numbers[1,1])
		local level_1 = sum(numbers[2,1])
		local level_2 = sum(numbers[3,1])
		local level_3 = sum(numbers[4,1])

		
		local level_0_prop = (`level_0'/r(N)*100)
		local level_1_prop = (`level_1'/r(N)*100)
		local level_2_prop = (`level_2'/r(N)*100)
		local level_3_prop = (`level_3'/r(N)*100)

			
	local var_label: var label `var'

	
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_0' E`x'=`level_0_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_1' E`x'=`level_1_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_2' E`x'=`level_2_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_3' E`x'=`level_3_prop'
	
}

local x=14
foreach var in tdi_cat {
	
	local x=`x'+1	
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
	
	tab `var', matcell(numbers)
	
		local level_0 = sum(numbers[1,1])
		local level_1 = sum(numbers[2,1])
		local level_2 = sum(numbers[3,1])
		local level_3 = sum(numbers[4,1])
		local level_4 = sum(numbers[5,1])
		
		local level_0_prop = (`level_0'/r(N)*100)
		local level_1_prop = (`level_1'/r(N)*100)
		local level_2_prop = (`level_2'/r(N)*100)
		local level_3_prop = (`level_3'/r(N)*100)
		local level_4_prop = (`level_4'/r(N)*100)

			
	local var_label: var label `var'

	
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_0' E`x'=`level_0_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_1' E`x'=`level_1_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_2' E`x'=`level_2_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_3' E`x'=`level_3_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Categorical"  D`x'=`level_4' E`x'=`level_4_prop'	
}	

local x=19
foreach var in age sd_age bmi_0_0 sd_bmi tdi_0_0 sd_tdi {
	
	local x=`x'+1	
	putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
	
	summ `var'
			
	local var_label: var label `var'

	putexcel A`x'="`var_label'" B`x'="Continuous"  D`x'=`r(mean)' E`x'=`r(sd)'

}	

* Add variable levels
putexcel set 20210409_UKBB_simulations_infection_`sampleDef', sheet(prevalence_and_distributions) modify
putexcel C2="Female" C3="Male" C4="Not tested/test negative for COVID-19 before mass testing" C5="Tested for COVID-19 before mass testing" C6="Not tested positive for COVID-19 before mass testing" C7="Tested positive for COVID-19 before mass testing" C8="Non-smoker" C9="Former smoker" C10="Current smoker" C11="GCSE/O levels or less" C12="AS or A levels" C13" Vocational qualification" C14="Degree or higher" C15="1 - Least deprived" C16="2" C17="3" C18="4" C19="5 - Most deprived"
