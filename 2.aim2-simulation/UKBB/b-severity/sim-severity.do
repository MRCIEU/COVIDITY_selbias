


*** Louise AC Millard August 2020
***

clear
graph drop _all

local covidSelectOR = "`1'"
di "`covidSelectOR'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"

log using "out/log-`bmiEffect'-`selInteractEffect'.txt", text replace

set seed 1234

file open myfile using "out/sim-`bmiEffect'-`selInteractEffect'.csv", write replace
file open myfile2 using "out/sim-`bmiEffect'-`selInteractEffect'-summaries.csv", write replace


file write myfile "iter,strata,estimate,lower,upper,n,conv" _n
file write myfile2 "iter,strata,mean" _n


* number of people in UKB sample
local n = 421122

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
	*** covid risk - 3.16% – from external source

        gen covidRiskPart = -0.2221*education_alevel + 0.1172*education_voc + -0.2043*education_degree + 0.2838*sex_m + -0.0702*sd_age + 0.0436*smoking_previous -0.2052*smoking_current + 0.1231*sd_tdi + -3.526
	
	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = runiform() <= pCovid

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
	*** selection - 4.329% are selected into our sample (have had a covid test taken)
	*** risk ratio = 5.05 for effect of SARS-CoV-2 infection on being tested

	* generate probability of being selected using Poisson model
        if ("`selInteractEffect'" == "nointeract") {

                gen logp = 0.1617*sd_bmi + -0.2084*education_alevel + -0.0105*education_voc + -0.1443*education_degree + -0.1079*sex_m + 0.1047*sd_age + 0.1625*smoking_previous + 0.2835*smoking_current + 0.2088*sd_tdi + log(5.05)*covid + XXXX

        }
	else if ("`selInteractEffect'" == "plausible") {

                gen covidBMIinteract = sd_bmi*covid
		gen logp = XXXX*sd_bmi + XXXX*education_alevel + XXXX*education_voc + XXXX*education_degree + XXXX*sex_m + XXXX*sd_age + XXXX*smoking_previous + XXXX*smoking_current +	XXXX*sd_tdi + XXXX*covid + XXXX*covidBMIinteract + XXXX

        }


	# generate selection variable - whether you got a test
        gen pSel = exp(logp)
        summ pSel
        gen selection1 = rbinomial(1,pSel)


	* all those who died with covid were tested so set these to selected
	gen selection = selection1

	if ("`setup'" == "all" | strpos("`setup'","severity")>0) {
		di "generate selection with severity"
		replace selection = 1 if covidSeverity == 1
	}
	
	***
	*** tidy and check distributions
	
	drop logitSelectPart covidRiskPart pSelect pCovid
	
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









