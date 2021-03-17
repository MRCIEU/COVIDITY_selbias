/*

A. R. Carter - 09/03/2021
Generating parameter estimates in UK Biobank to inform simulations of COVID-19 severity in UK Biobank
This script uses data generates on the 04/03/2021, including COVID-19 test results until February 2021. 
However, these simulations focus only on the pre-mass testing phase of the pandemic, therefore two variables are created in this script to define deaths and tests from the pre-mass testing phase

*/

use "$resDir/data/COVIDITY/UKBB/covidity_data_20210304.dta", clear
cd "$resDir/results/COVIDITY/UKBB/for_simulations"


* Restrict analysis dates to pre mass testing (18th May 2020)
* mass_test variable defined according to date of first test (defined in defining_covid_cases.do)

* Cases/tests pre mass testing have the suffix _phase1 - note that this definition is based on testing dates where phase1 means the pre-mass testing phase, until the 28th May
	
*Association of covariates with BMI	in whole UKBB sample
regress sd_bmi i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi
matrix results = r(table)
matrix results = results[1..6,1..12]
	putexcel set 20210309_UKBB_simulations_severity.xlsx, sheet(bmi_outcome_all) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
	putexcel A10="F stat" B10=`e(F)'
	
*Association of covariates with covid positive in tested sample
* Create variable to define covid positive cases in the first wave
* This uses the variable mass_test which identifies whether the participant had their first COVID-19 test before or after mass tsting began on the 18th May 2020, but specifies in line 21 that the first_positive_test must also have been received before the mass testing changes. If the participant had a negative COVID-19 test in the pre-mass testing period, but only tested positive in the post mass testing period, they would be defined as a control here
gen covid_positive_phase1 = .
replace covid_positive_phase1 = 1 if covid_positive==1 & mass_test==0 & first_positive_test <date("20200518", "YMD")
replace covid_positive_phase1 = 0 if covid_positive_phase1==.
lab var covid_positive_phase1 "Tested positive for COVID-19 before mass testing"
lab def covid_positive_phase1 0 "COVID-19 negative before 18/5/20" 1 "COVID-19 positive before 18/5/20", modify
lab val covid_positive_phase1 covid_positive_phase1

logistic covid_positive_phase1 i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi if mass_test==0, coef
matrix results = r(table)
matrix results = results[1..6,1..12]
	putexcel set 20210309_UKBB_simulations_severity.xlsx, sheet(covid_outcome_tested) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
	
* Association of BMI/covariates with dying from covid in the covid positive subsample
* Create variable to define covid pdeaths in the first wave
* This code uses the variable covid_positive_phase1 (as defined above) to define the analysis sample in the regression model 
gen covid_death_phase1 = .
replace covid_death_phase1 = 1 if covid_death==1 & date_of_death <date("20200518", "YMD")
replace covid_death_phase1 = 0 if covid_death_phase1==.
lab var covid_death_phase1 "Died from COVID-19 before mass testing"
lab def covid_death_phase1 1 "Died from COVID-19 before 18/5/20" 0 "Alive before 18/5/20", modify
lab val covid_death_phase1 covid_death_phase1

logistic covid_death_phase1 sd_bmi i.eduyears_quali i.sex sd_age i.current_smoke sd_tdi if covid_positive_phase1==1, coef
matrix results = r(table)
matrix results = results[1..6,1..13]
	putexcel set 20210309_UKBB_simulations_severity.xlsx, sheet(death_outcome_testpositive) modify
	putexcel A1 = matrix(results), names nformat(number_d2)
		
* Prevalence and distribution of variables

putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
putexcel A1="Variable" B1="Variable type" C1="Level" D1="Prevalence/mean" E1="percent/SD"	

local x=1
foreach var in sex covid_positive_phase1 covid_death_phase1 {

	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
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
foreach var in covid_death_phase1 {

	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
	tab `var' if covid_positive_phase1==1, matcell(numbers)
	
		local control = sum(numbers[1,1])
		local case = sum(numbers[2,1])
		
		local control_prop = (`control'/r(N)*100)
		local case_prop = (`case'/r(N)*100)
			
	local var_label: var label `var'

	
	putexcel A`x'="`var_label'" B`x'="Binary"  D`x'=`control' E`x'=`control_prop'
	local x=`x'+1
	putexcel A`x'="`var_label'" B`x'="Binary"  D`x'=`case' E`x'=`case_prop'

	}
local x=9
foreach var in current_smoke {
	
	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
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

local x=12
foreach var in eduyears_quali {
	
	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
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

local x=16
foreach var in tdi_cat {
	
	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
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

local x=21
foreach var in age sd_age bmi_0_0 sd_bmi tdi_0_0 sd_tdi {
	
	local x=`x'+1	
	putexcel set 20210309_UKBB_simulations_severity, sheet(prevalence_and_distributions) modify
	
	summ `var'
			
	local var_label: var label `var'

	putexcel A`x'="`var_label'" B`x'="Continuous"  D`x'=`r(mean)' E`x'=`r(sd)'

}	
