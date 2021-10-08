*** Louise AC Millard August 2020
***


clear


local bmiEffect = "`1'"
di "BMI affects covid risk: `bmiEffect'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"

local wide = "`3'"

**
** load in the simulation results and plot distributions

graph drop _all

di "out/sim-`bmiEffect'-`selInteractEffect'.csv"

insheet using "out/sim-`bmiEffect'-`selInteractEffect'.csv"


summ


set scheme s1mono

* set xrange depending on the simulation
if ("`bmiEffect'" == "effect") {
*	local xmin = -0.2
*	local xmax = 2.6

	if ("`wide'" == "1") {
		local xmin = 0.4
		local xmax = 2.0
	}
	else {
		local xmin = 0.7
		local xmax = 1.4
	}
}
else {

	if ("`wide'" ==	"1") {
		local xmin = -0.8
		local xmax = 0.8
	}
	else {
		local xmin = -0.4
		local xmax = 0.4
	}
}




di "xmin `xmin' xmax `xmax'"

twoway (histogram estimate if strata == "all", color(eltblue%40)) ///        
	(histogram estimate if strata == "all-confadj", color(ebblue%40)) ///
	(histogram estimate if strata == "selected", color(teal%40)) ///   
	(histogram estimate if strata == "selected-confadj", color(midgreen%40)) ///
	(histogram estimate if strata == "control-everyone", color(purple%20)), ///
	legend(order(1 "All" 2 "All: conf adjusted" 3 "Selected" 4 "Selected: conf adj" 5 "All controls=everyone")) ///
	xscale(r(`xmin' `xmax')) xlabel(`xmin'(0.2)`xmax') ///
	xtitle("Log odds")


graph export "out/sim-main-`bmiEffect'-`selInteractEffect'.pdf", replace
