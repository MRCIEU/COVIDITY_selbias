


*** Louise AC Millard August 2020
***
*** Full model, where everything affects selection


clear
graph drop _all

local bmiEffect = "`1'"
di "BMI affects covid risk: `bmiEffect'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"

local largeN = "`3'"

local jobarray = `4'


* set random numbers using streams and seeds
set rng mt64s
local streamNum = 4000+`jobarray'
di "stream number: `streamNum'"
set rngstream `streamNum'

set seed 1234


log using "out/log-`bmiEffect'-`selInteractEffect'`largeN'-`jobarray'.txt", text replace

file open myfile using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'-`jobarray'.csv", write replace
file open myfile2 using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'-summaries-`jobarray'.csv", write replace
file open myfile3 using "out/sim-`bmiEffect'-`selInteractEffect'`largeN'-checking-`jobarray'.csv", write replace

file write myfile "iter,strata,n,estimate,lower,upper" _n
file write myfile2 "iter,variable,mean,min,max" _n
file write myfile3 "iter,param,beta,lower,upper" _n


if ("`largeN'" == "1") {
	local n = 5000000
	di "N LARGE 5000000"
}
else {
	local n = 14849
	di "N 14849"
}


local i = 1
local nSim = 50

while `i'<=`nSim' {
		
	di "***"	
	di `i'

	set obs `n'
	
	* set up confounders
	gen edu_rand = runiform()
	gen education_gcse = edu_rand < 0.1169
	gen education_voc = edu_rand >= 0.1169 & edu_rand < 0.1169+0.1394
	gen education_alevel = edu_rand >= 0.1169+0.1394 & edu_rand < 0.1169+0.1394+0.2789
	gen education_degree = edu_rand >= 0.1169+0.1394+0.2789
	drop edu_rand

	gen sex_m = runiform() < 0.3226
	gen sd_age = rnormal(0,1)

	* smoking - never versus current versus previous
	gen smoke_rand = runiform()
	gen smoking_current = smoke_rand < 0.2524
	gen smoking_previous = smoke_rand >=0.2524 & smoke_rand < 0.2524+0.3595

	gen imd_rand = runiform()
	gen imd = 1 if imd_rand < 0.3841
	replace imd = 2 if imd_rand >=0.3841 & imd_rand < 0.3841 + 0.2587
	replace imd = 3 if imd_rand >=0.3841+0.2587 & imd_rand < 0.3841 + 0.2587 + 0.1717
	replace imd = 4 if imd_rand >=0.3841+0.2587+0.1717 & imd_rand < 0.3841 + 0.2587 + 0.1717 + 0.1263
	replace imd = 5 if imd_rand >=0.3841+0.2587+0.1717+0.1263



	* WE USE SD BMI
	gen sd_bmi =  -0.1502*education_alevel + 0.1064*education_voc + -0.2645*education_degree + 0.0409*sex_m + 0.0044*sd_age + 0.1202*smoking_previous + 0.0346*smoking_current + 0.0764*imd -0.084 + rnormal(0,0.985)
*	regress sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current imd
		

	***
	*** covid risk - 7.20% - from external source

	if ("`bmiEffect'" == "effect") {
		gen covidRiskPart = log(3)*sd_bmi + 0.3179*education_alevel + 0.3313*education_voc + 0.3436*education_degree + 0.1944*sex_m + 0.0297*sd_age + 0.3287*smoking_previous + 0.2191*smoking_current + -0.0497*imd + -3.475
	}
	else if ("`bmiEffect'" == "null") {
		gen covidRiskPart = 0.3179*education_alevel + 0.3313*education_voc + 0.3436*education_degree + 0.1944*sex_m + 0.0297*sd_age + 0.3287*smoking_previous + 0.2191*smoking_current + -0.0497*imd + -2.997
	}

	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = rbinomial(1,pCovid) 
	

	***
	*** selection - 19.974% are selected into our sample (responded to first covid questionnaire)
	*** risk ratio = 5.80 for effect of SARS-CoV-2 infection on being tested
	
	* needed to decrease intercept to -2.43 so that pSelx in range [0,1]
	gen logpx = 0.0208*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  -0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd  + log(5.80)*covid +  -2.75

 	gen pSelx = exp(logpx)
        summ pSelx

	count if pSelx>1
	if (`r(N)'>0) {
		di "SIM ISSUE: invalid probabilities"
	}

        gen selectionx = rbinomial(1,pSelx)
	

	if ("`selInteractEffect'" == "plausible") {

		* interaction size is 0.0527

		local b1=0.005
		local b2=`b1' + 0.0527

		summ sd_bmi if covid == 0
		local mu0 = `r(mean)'
		summ sd_bmi if covid == 1
		local mu1 = `r(mean)'

		local b = 0.0208
		local a0 = -2.75
		local c0 = log(5.80)

		local A = `a0' + `b'*`mu0'
		local B = `a0' + `c0' + `b'*`mu1'

		local new_alpha0 = `A' - `b1'*`mu0'
		local c = `B' - `b2'*`mu1' - `new_alpha0'

		gen logpnew = `b1'*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  -0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd + `new_alpha0' if covid == 0
		replace logpnew = `b2'*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  -0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd  + `c' + `new_alpha0' if covid == 1

		* generate selection variable
	        gen pSel = exp(logpnew)
	        summ pSel
		
		count if pSel>1
        	if (`r(N)'>0) {
        	        di "SIM	ISSUE: invalid probabilities (plausible)" 
        	}

	        gen selection = rbinomial(1,pSel)

		do ../checking.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current imd"
		
		drop logpnew
        }
	else if ("`selInteractEffect'" == "extreme") {

		* interaction size is 0.135

		* b1 chosen to get the right params of poisson model
		local b1=-0.019
		local b2=`b1' + 0.135

		summ sd_bmi if covid == 0
		local mu0 = `r(mean)'
		summ sd_bmi if covid == 1
		local mu1 = `r(mean)'

		local b = 0.0208
		local a0 = -2.75
		local c0 = log(5.80)

		local A = `a0' + `b'*`mu0'
		local B = `a0' + `c0' + `b'*`mu1'

		local new_alpha0 = `A' - `b1'*`mu0'
		local c = `B' - `b2'*`mu1' - `new_alpha0'

		gen logpnew = `b1'*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  -0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd + `new_alpha0' if covid == 0
                replace logpnew = `b2'*sd_bmi + 0.3387*education_alevel + 0.1095*education_voc + 0.3408*education_degree +  -0.2689*sex_m + 0.0162*sd_age + -0.0228*smoking_previous + -0.0594*smoking_current + -0.0175*imd  + `c' + `new_alpha0' if covid == 1

		* generate selection variable
	        gen pSel = exp(logpnew)
	        summ pSel

		count if pSel>1
                if (`r(N)'>0) {
                        di "SIM ISSUE: invalid probabilities (extreme)"
                }

	        gen selection = rbinomial(1,pSel)

		do ../checking.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current imd"

		drop logpnew
	}
	else {
		gen selection = selectionx
	}

	***
	*** tidy and check distributions
	
	drop covidRiskPart logpx
	
	summ
	

	***
	*** store variable summaries so we can check they are on average the right proportions / means

	do ../association-tests.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current imd"
	
	local i=`i'+ 1
	
	drop _all

}

file close myfile
file close myfile2
file close myfile3


log close









