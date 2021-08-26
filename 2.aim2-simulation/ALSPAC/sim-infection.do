


*** Louise AC Millard August 2020
***
*** Full model, where everything affects selection


clear
graph drop _all

local bmiEffect = "`1'"
di "BMI affects covid risk: `bmiEffect'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"

log using "out/log-`bmiEffect'-`selInteractEffect'.txt", text replace

set seed 1234

file open myfile using "out/sim-`bmiEffect'-`selInteractEffect'.csv", write replace
file open myfile2 using "out/sim-`bmiEffect'-`selInteractEffect'-summaries.csv", write replace

file write myfile "iter,strata,estimate,lower,upper" _n
file write myfile2 "iter,strata,mean" _n

local n = 14849

local i = 1
local nSim = 1000

while `i'<=`nSim' {
		
	di "***"	
	di `i'
	set obs `n'
	
	* set up confounders
	gen edu_rand = uniform()
	gen education_gcse = edu_rand < 0.1361
	gen education_voc = edu_rand >= 0.1361 & edu_rand < 0.1361+0.1495
	gen education_alevel = edu_rand >= 0.1361+0.1495 & edu_rand < 0.1361+0.1495+0.2700
	gen education_degree = edu_rand >= 0.1361+0.1495+0.2700
	drop edu_rand

	gen sex_m = uniform() < 0.5104
	gen sd_age = rnormal(0,1)

	* smoking - never versus current versus previous
	gen smoke_rand = uniform()
	gen smoking_current = smoke_rand < 0.3003
	gen smoking_previous = smoke_rand >=0.3003 & smoke_rand < 0.3003+0.3322

	gen imd_rand = uniform()
	gen imd = 1 if imd_rand < 0.2986
	replace imd = 2 if imd_rand >=0.2986 & imd_rand < 0.2986 + 0.2281
	replace imd = 3 if imd_rand >=0.2986+0.2281 & imd_rand < 0.2986 + 0.2281 + 0.1808
	replace imd = 4 if imd_rand >=0.2986+0.2281+0.1808 & imd_rand < 0.2986 + 0.2281 + 0.1808 + 0.1656
	replace imd = 5 if imd_rand >=0.2986+0.2281+0.1808+0.1656



	* WE USE SD BMI
	gen sd_bmi =  -0.1502*education_alevel + 0.1064*education_voc + -0.2645*education_degree + 0.0409*sex_m + 0.0044*sd_age + 0.1202*smoking_previous + 0.0346*smoking_current + 0.0764*imd -0.127 + rnormal(0,0.99)
*	regress sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi
		

	***
	*** covid risk - 7.20% - from external source

	if ("`bmiEffect'" == "effect") {
		gen covidRiskPart = log(3)*sd_bmi + 0.3179*education_alevel + 0.3313*education_voc + 0.3436*education_degree + 0.1944*sex_m + 0.0297*sd_age + 0.3287*smoking_previous + 0.2191*smoking_current + -0.0497*imd + -3.4846
	}
	else if ("`bmiEffect'" == "null") {
		gen covidRiskPart = 0.3179*education_alevel + 0.3313*education_voc + 0.3436*education_degree + 0.1944*sex_m + 0.0297*sd_age + 0.3287*smoking_previous + 0.2191*smoking_current + -0.0497*imd + -3.0098
	}

	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = rbinomial(1,pCovid) 
	

	***
	*** selection - 19.974% are selected into our sample (responded to first covid questionnaire)
	*** risk ratio = 5.80 for effect of SARS-CoV-2 infection on being tested
	
	* generate probability of being selected using Poisson model
	if ("`selInteractEffect'" == "nointeract") {

		gen logp = 0.0208*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd  + log(5.80)*covid + XXXX

	}
	else if ("`selInteractEffect'" == "plausible") {

		gen covidBMIinteract = sd_bmi*covid
		gen logp = XXXX*sd_bmi + XXXX*education_alevel + XXXX*education_voc + XXXX*education_degree + XXXX*sex_m + XXXX*sd_age + XXXX*smoking_previous + XXXX*smoking_current + XXXX*imd  + XXXX*covid + XXXX*covidBMIinteract * XXXX
		
	}
	else if ("`selInteractEffect'" == "extreme") {

                gen covidBMIinteract = sd_bmi*covid
                gen logp = XXXX*sd_bmi + XXXX*education_alevel + XXXX*education_voc + XXXX*education_degree + XXXX*sex_m + XXXX*sd_age + XXXX*smoking_previous + XXXX*smoking_current + XXXX*imd  + XXXX*covid + XXXX*covidBMIinteract * XXXX

        }	

	# generate selection variable
	gen pSel = exp(logp)
	summ pSel
	gen selection = rbinomial(1,pSel)
	
	
	***
	*** tidy and check distributions
	
	drop logitSelectPart covidRiskPart pSelect pCovid
	drop covidBMIinteract logp pSel pCovid
	
	summ
	

	***
	*** store variable summaries so we can check they are on average the right proportions / means

	do ../association-tests.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current imd"
	
	local i=`i'+ 1
	
	drop _all

}

file close myfile
file close myfile2


log close









