


*** Louise AC Millard August 2020
***
*** Full model, where everything affects selection

*** in this version BMI has an effect on COVID19 risk

clear
graph drop _all

local setup = "`1'"
di "Setup: `setup'"

local covidSelectOR = "`2'"
di "OR of covid infection effect on selection: `covidSelectOR'"

local bmiEffect = "`3'"
di "BMI affects covid risk: `bmiEffect'"

log using "out/log-`bmiEffect'-`setup'-`covidSelectOR'.txt", text replace

set seed 1234

file open myfile using "out/sim-`bmiEffect'-`setup'-`covidSelectOR'.csv", write replace
file open myfile2 using "out/sim-`bmiEffect'-summaries-`setup'-`covidSelectOR'.csv", write replace

file write myfile "iter,strata,estimate,lower,upper" _n
file write myfile2 "iter,strata,mean" _n


* number of people in UKB sample
local n = 421122

local i = 1
local nSim = 1000

while `i'<=`nSim' {
			
	di $i
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

	* SD BMI 
	gen sd_bmi =  -0.1561*education_alevel + -0.0290*education_voc + -0.2739*education_degree + 0.1606*sex_m + 0.0214*sd_age + 0.1049*smoking_previous - 0.1074*smoking_current + 0.0881*sd_tdi + 0.0106 + rnormal(0,0.983)
	*regress sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi
	
		

	***
	*** covid risk - 3.16% – from external source

	if ("`bmiEffect'" == "effect") {
		gen covidRiskPart = log(3)*sd_bmi + -0.2221*education_alevel + 0.1172*education_voc + -0.2043*education_degree + 0.2838*sex_m + -0.0702*sd_age + 0.0436*smoking_previous -0.2052*smoking_current + 0.1231*sd_tdi + -4.100
	}
	else if ("`bmiEffect'" == "null") {
		gen covidRiskPart = -0.2221*education_alevel + 0.1172*education_voc + -0.2043*education_degree + 0.2838*sex_m + -0.0702*sd_age + 0.0436*smoking_previous -0.2052*smoking_current + 0.1231*sd_tdi + -3.526
	}

	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = runiform() <= pCovid
		
	
	***
	*** selection - 4.329% are selected into our sample (have had a covid test taken)

	if ("`setup'" == "all") {
		di "generate selection with all indep vars"
		if ("`covidSelectOR'" == "2") {
			gen logitSelectPart = 0.1641*sd_bmi + -0.2110*education_alevel + -0.0107*education_voc + -0.1461*education_degree + 0.1091*sex_m + 0.1061*sd_age + 0.1645*smoking_previous + 0.2874*smoking_current + 0.2118*sd_tdi + log(2)*covid + -3.253
		}
		else if ("`covidSelectOR'" == "5") {
			gen logitSelectPart = 0.1641*sd_bmi + -0.2110*education_alevel + -0.0107*education_voc + -0.1461*education_degree + 0.1091*sex_m + 0.1061*sd_age + 0.1645*smoking_previous + 0.2874*smoking_current + 0.2118*sd_tdi + log(5)*covid + -3.340
		}
		else if	("`covidSelectOR'" == "10") {
			gen logitSelectPart = 0.1641*sd_bmi + -0.2110*education_alevel + -0.0107*education_voc + -0.1461*education_degree + 0.1091*sex_m + 0.1061*sd_age + 0.1645*smoking_previous + 0.2874*smoking_current + 0.2118*sd_tdi + log(10)*covid + -3.441
		}

	}
	else if ("`covidSelectOR'" == "2") {
		* only run partial versions for main OR=2 option
	
	if ("`setup'" == "bmi") {
		di "generate selection with bmi only"
		gen logitSelectPart = 0.1435*sd_bmi + -3.311
	}
	else if	("`setup'" == "covars") {
		di "generate selection with covars only"
		gen logitSelectPart =  -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + -3.311
        }
	else if	("`setup'" == "covid") {
		di "generate selection with covid only"
		gen logitSelectPart = log(2)*covid + -3.311
        }
	else if	("`setup'" == "bmi_covars") {
		di "generate selection with bmi and covars"
		gen logitSelectPart = 0.1435*sd_bmi + -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + -3.311
        }
	else if ("`setup'" == "bmi_covid") {
		di "generate selection with bmi and covid"
		gen logitSelectPart = 0.1435*sd_bmi + log(2)*covid + -3.311
        }
	else if ("`setup'" == "covars_covid") {
		di "generate selection with covars and covid
		gen logitSelectPart = -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + log(2)*covid + -3.311
        }
	else {
		di "Valid setup not specified : `setup'"
		exit, clear
	}
	}

	gen pSelect=exp(logitSelectPart)/(1+exp(logitSelectPart))
	gen selection = runiform() <= pSelect
	
	
	
	***
	*** tidy and check distributions
	
	drop logitSelectPart covidRiskPart pSelect pCovid
	
	summ
	
	
	***
	*** store variable summaries so we can check they are on average the right proportions / means

	do ../../association-tests.do `i' "education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi"
	
	local i=`i'+ 1
	
	drop _all

}

file close myfile
file close myfile2

log close









