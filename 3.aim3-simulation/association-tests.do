


*** Louise AC Millard August 2020
***

* iteration number
local i = `1'

* covariate names
local covars = "`2'"



	summ sd_bmi
	file write myfile2 %04.0f (`i') ",sd_bmi," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ sd_bmi if covid == 0
	file write myfile2 %04.0f (`i') ",sd_bmi_nocovid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ sd_bmi if covid == 1
	file write myfile2 %04.0f (`i') ",sd_bmi_covid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ covid
	file write myfile2 %04.0f (`i') ",covid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ selection
	file write myfile2 %04.0f (`i') ",selection," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ selectionx
        file write myfile2 %04.0f (`i') ",selectionx," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ covid if selection == 1
	file write myfile2 %04.0f (`i') ",covid_selection," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ sd_bmi if selection == 1
        file write myfile2 %04.0f (`i') ",sd_bmi_selection," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ sd_bmi if selection == 1 & covid == 0
        file write myfile2 %04.0f (`i') ",sd_bmi_selection_nocovid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ sd_bmi if selection == 1 & covid == 1
        file write myfile2 %04.0f (`i') ",sd_bmi_selection_covid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n	

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
	file write myfile %04.0f (`i') ",all," (`e(N)') "," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n

	* test assoc in whole sample adjusted for confounders
	di "assoc in whole sample adjusted for confounders"
	logistic covid sd_bmi `covars', coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	file write myfile %04.0f (`i') ",all-confadj," (`e(N)') "," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n	

	* test assoc in subsample 
	di "assoc in selected sub sample"
	logistic covid sd_bmi if selection == 1, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	file write myfile %04.0f (`i') ",selected," (`e(N)') "," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n

	* test assoc in subsample - only those tested for COVID, adjusted for confounders
	di "assoc in selected sub sample adjusted for confounders"
	logistic covid sd_bmi `covars' if selection == 1, coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	file write myfile %04.0f (`i') ",selected-confadj," (`e(N)') "," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n
	
	* test with different controls: case=those +ve AND selected, control=everyone else
	gen covidControlEveryone = covid==1 & selection==1
	
	di "assoc in whole sample with cases=those +ve AND selected, control=everyone else"
	logistic covidControlEveryone sd_bmi `covars', coef
	local beta _b[sd_bmi]
	local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
	local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
	file write myfile %04.0f (`i') ",control-everyone," (`e(N)') "," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n
	




