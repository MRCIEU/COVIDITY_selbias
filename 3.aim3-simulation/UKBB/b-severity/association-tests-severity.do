


*** Louise AC Millard August 2020
***

* iteration number
local i = `1'

* covariate names
local covars = "`2'"



	summ sd_bmi
	file write myfile2 %04.0f (`i') ",sd_bmi," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ covid
	file write myfile2 %04.0f (`i') ",covid," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ covidSeverity
	file write myfile2 %04.0f (`i') ",covidseverity," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ selection1
        file write myfile2 %04.0f (`i') ",selection1," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ selection
	file write myfile2 %04.0f (`i') ",selection," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n
	summ selectionx
        file write myfile2 %04.0f (`i') ",selectionx," %7.6f (`r(mean)') "," %7.6f (`r(min)') "," %7.6f (`r(max)') _n

	***
	*** association tests

	di "***"
	di "*** association tests"
	di "***"

	* test assoc in whole sample
	di "assoc in whole sample"
	capture logistic covidSeverity sd_bmi, coef iterate(100)	
	
	if _rc == 0 {
		local beta _b[sd_bmi]
		local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
		local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
		file write myfile %04.0f (`i') ",all," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') "," %8.0g (e(N)) "," %01.0f (e(converged)) _n
	}

	* test assoc in whole sample adjusted for confounders
	di "assoc in whole sample adjusted for confounders"
	capture logistic covidSeverity sd_bmi `covars', coef iterate(100)

	if _rc == 0 {
		local beta _b[sd_bmi]
		local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
		local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
		file write myfile %04.0f (`i') ",all-confadj," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') "," %8.0g (e(N)) "," %01.0f (e(converged)) _n
	}

	* test assoc in subsample 
	di "assoc in selected sub sample"
	capture logistic covidSeverity sd_bmi if selection == 1, coef iterate(100)

	if _rc == 0 {
		local beta _b[sd_bmi]
		local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
		local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
		file write myfile %04.0f (`i') ",selected," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') "," %8.0g (e(N)) "," %01.0f (e(converged)) _n
	}

	* test assoc in subsample - only those tested for COVID, adjusted for confounders
	di "assoc in selected sub sample adjusted for confounders"
	capture logistic covidSeverity sd_bmi `covars' if selection == 1, coef iterate(100)

	if _rc == 0 {
		local beta _b[sd_bmi]
		local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
		local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
		file write myfile %04.0f (`i') ",selected-confadj," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') "," %8.0g (e(N)) "," %01.0f (e(converged)) _n
	}


	* test with different controls: case=those +ve AND selected, control=everyone else
	gen covidSeverityControlEveryone = covidSeverity==1 & selection==1

	di "assoc in whole sample with cases=those +ve AND selected, control=everyone else"
	capture logistic covidSeverityControlEveryone sd_bmi `covars', coef iterate(100)

	if _rc == 0 { 
		local beta _b[sd_bmi]
		local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
		local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
		file write myfile %04.0f (`i') ",control-everyone," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') "," %8.0g (e(N)) "," %01.0f (e(converged)) _n
	}



