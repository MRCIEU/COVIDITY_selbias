*** Louise AC Millard August 2020
***


clear


* either effect or null for simulations where bmi has an effect or no effect on covid risk (the hypothesis we are testing)
local bmi_assoc = "`1'"
di "`bmi_assoc'"

* relates to the edges including in the dag (specifically what has a direct effect on selection)
local setup = "`2'"
di "`setup'"

* OR of covid on selection
local covidSelectOR = "`3'"
di "`covidSelectOR'"


**
** load in the simulation results and plot distributions

graph drop _all

use "out/sim-main-`bmi_assoc'-`setup'-`covidSelectOR'.dta", clear


summ

*replace estimate = log(estimate)

set scheme s1mono

* set xrange depending on the simulation
if ("`bmi_assoc'" == "effect") {
	local xmin = 0.9
	local xmax = 1.1
}
else {
	local xmin = -0.1
	local xmax = 0.2
}


di "xmin `xmin' xmax `xmax'"

twoway (histogram estimate if strata == "all", color(red%30)) ///        
	(histogram estimate if strata == "all-confadj", color(blue%30)) ///
	(histogram estimate if strata == "selected", color(green%30)) ///   
	(histogram estimate if strata == "selected-confadj", color(grey%30)) ///
	(histogram estimate if strata == "control-everyone", color(purple%30)), ///
	legend(order(1 "All" 2 "All: conf adjusted" 3 "Selected" 4 "Selected: conf adj" 5 "All controls=everyone")) ///
	xscale(r(`xmin' `xmax')) xlabel(`xmin'(0.05)`xmax')


graph export "out/sim-main-`bmi_assoc'-`setup'-`covidSelectOR'.pdf", replace
