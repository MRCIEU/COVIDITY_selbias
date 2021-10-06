


*** Louise AC Millard August 2020
***

clear
graph drop _all

local bmiEffect = "`1'"
di "BMI affects covid risk: `bmiEffect'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"

local largeN = "`3'"

log using "out/log-`bmiEffect'-`selInteractEffect'`largeN'.txt", text replace

set seed 1234

file open myfile using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'.csv", write replace
file open myfile2 using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'-summaries.csv", write replace
file open myfile3 using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'-checking.csv", write replace

file write myfile "iter,strata,estimate,lower,upper,n,conv" _n
file write myfile2 "iter,variable,mean,min,max" _n
file write myfile3 "iter,param,beta,lower,upper" _n


* number of people in UKB sample
if ("`largeN'" == "1") {
        local n = 5000000
        di "N LARGE 5000000"
}
else {
      	local n = 421037
        di "N 421037"
}


local i = 1
local nSim = 1000

while `i'<=`nSim' {
			
	di `i'
	set obs `n'
	
	* set up confounders
	gen edu_rand = uniform()
	gen education_alevel = edu_rand < 0.0541
	gen education_voc = edu_rand >= 0.0541 & edu_rand < 0.0541+0.2803
	gen education_degree = edu_rand >= 0.0541+0.2803 & edu_rand < 0.0541+0.2803+0.3332
	drop edu_rand
	gen sex_m = uniform() < 0.4490
	gen sd_age = rnormal(0,1)
	gen sd_tdi = rnormal(0,1)

	* smoking - never versus current versus previous
	gen smoke_rand = uniform()
	gen smoking_current = smoke_rand < 0.0989
	gen smoking_previous = smoke_rand >=0.0989 & smoke_rand < 0.0989+0.3409

	* BMI - mean 27.4, sd 4.75
	gen sd_bmi =  -0.1561*education_alevel + -0.0290*education_voc + -0.2739*education_degree + 0.1606*sex_m + 0.0214*sd_age + 0.1049*smoking_previous - 0.1074*smoking_current + 0.0881*sd_tdi + 0.0106 + rnormal(0,0.983)
	*regress sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi
	

	***
	*** covid risk - 3.13% – from external source

        gen covidRiskPart = -0.2221*education_alevel + 0.1172*education_voc + -0.2043*education_degree + 0.2838*sex_m + -0.0702*sd_age + 0.0436*smoking_previous -0.2052*smoking_current + 0.1231*sd_tdi + -3.526
	
	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = rbinomial(1,pCovid)	

	***
	*** covid severity proxy – 3.13% of those with a SARS-CoV-2 infection died - from external source

	* params from sheet 'death_outcome_all' in Alice's xlsx

	if ("`bmiEffect'" == "effect") {
		gen deathCovidPart = log(3)*sd_bmi + -0.3682*education_alevel + -0.0496*education_voc + 0.0602*education_degree + 0.3475*sex_m + 0.9288*sd_age + 0.0261*smoking_previous + 0.5205*smoking_current + 0.0882*sd_tdi + -4.584
	}
	else if ("`bmiEffect'" == "null") {
	        gen deathCovidPart = -0.3682*education_alevel + -0.0496*education_voc + 0.0602*education_degree + 0.3475*sex_m + 0.9288*sd_age + 0.0261*smoking_previous + 0.5205*smoking_current + 0.0882*sd_tdi + -4.017
	}

        gen pCovidSeverity=exp(deathCovidPart)/(1+exp(deathCovidPart))
	gen covidSeverity = rbinomial(1,pCovidSeverity) if covid == 1
	
	***
	*** selection - 1.156% are selected into our sample (have had a covid test taken)
	*** risk ratio = 5.05 for effect of SARS-CoV-2 infection on being tested

	** covid model defined among those without and with covid separately
	* without covid
	gen logpx = 0.1617*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi -4.610 if covid == 0
	* with covid
	replace logpx = 0.1617*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + log(5.05) -4.610 if covid == 1


 	gen pSelx = exp(logpx)
        summ pSelx

	count if pSelx>1           
	if (`r(N)'>0) {
		di "SIM ISSUE: invalid probabilities (plausible)"
	}

        gen selectionx = rbinomial(1,pSelx)
	

	if ("`selInteractEffect'" == "plausible") {

		* interaction size is -0.162

		local b1=0.187
		local b2=`b1' - 0.162

		summ sd_bmi if covid == 0
		local mu0 = `r(mean)'
		summ sd_bmi if covid == 1
		local mu1 = `r(mean)'

		local b = 0.1617
		local a0 = -4.610
		local c0 = log(5.05)

		local A = `a0' + `b'*`mu0'
		local B = `a0' + `c0' + `b'*`mu1'

		local new_alpha0 = `A' - `b1'*`mu0'
		local c = `B' - `b2'*`mu1' - `new_alpha0'

		gen logpnew = `b1'*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + `new_alpha0' if covid == 0
		replace logpnew = `b2'*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + `c' + `new_alpha0' if covid == 1

		* generate selection variable
	        gen pSel = exp(logpnew)
	        summ pSel

		count if pSel>1
		if (`r(N)'>0) {
			di "SIM ISSUE: invalid probabilities (plausible)"
		}

	        gen selection = rbinomial(1,pSel)

		do ../../checking.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi"
		
		rename selection selection1
		
		drop logpnew pSel
        }
	else if ("`selInteractEffect'" == "extreme") {

		* interaction size is -0.245

		* b1 chosen to get the right params of poisson model
		local b1=0.205
		local b2=`b1' - 0.245

		summ sd_bmi if covid == 0
		local mu0 = `r(mean)'
		summ sd_bmi if covid == 1
		local mu1 = `r(mean)'

		local b = 0.1617
		local a0 = -4.610
		local c0 = log(5.05)

		local A = `a0' + `b'*`mu0'
		local B = `a0' + `c0' + `b'*`mu1'

		local new_alpha0 = `A' - `b1'*`mu0'
		local c = `B' - `b2'*`mu1' - `new_alpha0'


		gen logpnew = `b1'*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + `new_alpha0' if covid == 0
                replace logpnew = `b2'*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + `c' + `new_alpha0' if covid == 1


		* generate selection variable
	        gen pSel = exp(logpnew)
	        summ pSel

		count if pSel>1
		if (`r(N)'>0) {
			di "SIM ISSUE: invalid probabilities (plausible)"
		}

	        gen selection = rbinomial(1,pSel)

		do ../../checking.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi"

		rename selection selection1

		drop logpnew pSel

        }
	else if ("`selInteractEffect'" == "nointeract") {

		gen selection1 = selectionx

	}


	* all those who died with covid were tested so set these to selected
	gen selection = selection1

	if ("`setup'" == "all" | strpos("`setup'","severity")>0) {
		di "generate selection with severity"
		replace selection = 1 if covidSeverity == 1
	}
	
	***
	*** tidy and check distributions
	
	drop covidRiskPart pCovid deathCovidPart logp*
	
	summ
	
	
	***
	*** store variable summaries so we can check they are on average the right proportions / means

	do association-tests-severity.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi"
	
	local i=`i'+ 1
	
	drop _all

}

file close myfile
file close myfile2

log close









