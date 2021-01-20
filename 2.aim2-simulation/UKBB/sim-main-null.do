


*** Louise AC Millard August 2020
***
*** Full model, where everything affects selection


clear
graph drop _all

local setup = "`1'"
di "`setup'"


log using "out/log-null-`setup'.txt", text replace

set seed 1234

tempname memhold
postfile `memhold' str30 strata estimate lower upper using "out/sim-main-null-`setup'.dta" , replace

* number of people in UKB sample
local n = 421122

local i = 1
local nSim = 500

while `i'<=`nSim' {
		
	di "***"	
	di `i'
	set obs `n'
	
	* set up confounders
	gen edu_rand = uniform()
	gen education_alevel = edu_rand < 0.054
	gen education_voc = edu_rand >= 0.054 & edu_rand < 0.054+0.280
	gen education_degree = edu_rand >= 0.054+0.280 & edu_rand < 0.054+0.280+0.333
	drop edu_rand
	gen sex_m = uniform() < 0.45
	gen sd_age = rnormal(0,1)
	gen sd_tdi = rnormal(0,1)

	* smoking - never versus current versus previous
	gen smoke_rand = uniform()
	gen smoking_current = smoke_rand < 0.099
	gen smoking_previous = smoke_rand >=0.099 & smoke_rand < 0.099+0.332

	* BMI - mean 27.4, sd 4.75
	gen sd_bmi =  -0.1561*education_alevel + -0.0289*education_voc + -0.2739*education_degree + 0.1606*sex_m + 0.0215*sd_age + 0.1049*smoking_previous - 0.1075*smoking_current + 0.0881*sd_tdi - 0.0042 + rnormal(0,1)
	regress sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi
	
		

	***
	*** covid risk - 9.39% are tested positive, of those tested
	*** this assumes the effect of the covariates on covid risk is the same in the non-selected sample as the selected sample
	*** uses "Covid test result - Any positive test" results from ukb assocs
	gen covidRiskPart = -0.3787*education_alevel + 0.0440*education_voc + -0.2748*education_degree + 0.2557*sex_m + -0.2043*sd_age + 0.0507*smoking_previous -0.1405*smoking_current + 0.1682*sd_tdi + -2.2579
	gen pCovid=exp(covidRiskPart)/(1+exp(covidRiskPart))
	gen covid = runiform() <= pCovid

	logistic covid education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi, coef

		
	***
	*** selection - 4.33% are selected into our sample (have had a covid test taken)

	if ("`setup'" == "all") {
		di "generate selection with all indep vars"
		gen logitSelectPart = 0.1435*sd_bmi + -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + log(2)*covid + -3.2162
	}
	else if ("`setup'" == "bmi") {
		di "generate selection with bmi only"
		gen logitSelectPart = 0.1435*sd_bmi + -3.2162
	}
	else if	("`setup'" == "covars") {
		di "generate selection with covars only"
		gen logitSelectPart =  -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + -3.2162
        }
	else if	("`setup'" == "covid") {
		di "generate selection with covid only"
		gen logitSelectPart = log(2)*covid + -3.2162
        }
	else if	("`setup'" == "bmi_covars") {
		di "generate selection with bmi and covars"
		gen logitSelectPart = 0.1435*sd_bmi + -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + -3.2162
        }
	else if ("`setup'" == "bmi_covid") {
		di "generate selection with bmi and covid"
		gen logitSelectPart = 0.1435*sd_bmi + log(2)*covid + -3.2162
        }
	else if ("`setup'" == "covars_covid") {
		di "generate selection with covars and covid
		gen logitSelectPart = -0.0799*education_alevel + -0.0265*education_voc + -0.1348*education_degree + 0.1023*sex_m + 0.2273*sd_age + 0.1441*smoking_previous + 0.2977*smoking_current + 0.1190*sd_tdi + log(2)*covid + -3.2162
        }
	else {
		di "Valid setup not specified : `setup'"
		exit, clear
	}

	gen pSelect=exp(logitSelectPart)/(1+exp(logitSelectPart))
	gen selection = runiform() <= pSelect
	
	logistic selection sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi covid, coef
	
	
	***
	*** tidy and check distributions
	
	drop logitSelectPart covidRiskPart pSelect pCovid
	
	summ
	
	

	***
	*** association tests

	di "***"
	di "*** association tests"
	di "***"

	* test assoc in whole sample
	di "assoc in whole sample"
	logistic covid sd_bmi, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	post `memhold' ("all") (`beta') (`ciL') (`ciU')

	* test assoc in whole sample adjusted for confounders
	di "assoc in whole sample adjusted for confounders"
	logistic covid sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	post `memhold' ("all-confadj") (`beta') (`ciL') (`ciU')
	
	* test assoc in subsample 
	di "assoc in selected sub sample"
	logistic covid sd_bmi if selection == 1, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	post `memhold' ("selected") (`beta') (`ciL') (`ciU')

	* test assoc in subsample - only those tested for COVID, adjusted for confounders
	di "assoc in selected sub sample adjusted for confounders"
	logistic covid sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi if selection == 1, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	post `memhold' ("selected-confadj") (`beta') (`ciL') (`ciU')

	
	* test with different controls: case=those +ve AND selected, control=everyone else
	gen covidControlEveryone = covid==1 & selection==1
	
	di "assoc in whole sample with cases=those +ve AND selected, control=everyone else"
	logistic covidControlEveryone sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	post `memhold' ("control-everyone") (`beta') (`ciL') (`ciU')

	
	local i=`i'+ 1
	
	drop _all

}

postclose `memhold'


log close









