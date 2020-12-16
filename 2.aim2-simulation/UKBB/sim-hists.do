*** Louise AC Millard August 2020
***


clear


local bmi_assoc = "`1'"
di "`bmi_assoc'"

local setup = "`2'"
di "`setup'"


**
** load in the simulation results and plot distributions

graph drop _all
use "out/sim-main-`bmi_assoc'-`setup'.dta", clear

replace estimate = log(estimate)


set scheme s1mono
twoway (histogram estimate if strata == "all", color(red%30)) ///        
	(histogram estimate if strata == "all-confadj", color(blue%30)) ///
	(histogram estimate if strata == "selected", color(green%30)) ///   
	(histogram estimate if strata == "selected-confadj", color(grey%30)) ///
	(histogram estimate if strata == "control-everyone", color(purple%30)), ///
	legend(order(1 "All" 2 "All: conf adjusted" 3 "Selected" 4 "Selected: conf adj" 5 "All controls=everyone")) ///
	xscale(r(-0.1 0.2)) xlabel(-0.1(0.05)0.2)

graph export "out/sim-main-`bmi_assoc'-`setup'.pdf", replace

