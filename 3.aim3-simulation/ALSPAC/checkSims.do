
local bmiEffect = "`1'"
di "BMI affects covid risk: `bmiEffect'"

local selInteractEffect = "`2'"
di "Interaction effect of BMI/sars-cov-2 on selection: `selInteractEffect'"



if ("`selInteractEffect'" != "nointeract") {

** association of covid and bmi with selection

* beta for bmi should be ~0.0208
* beta for covid should be ~log(5.80) = 1.758

insheet using "out/sim-`bmiEffect'-`selInteractEffect'0-checking-all.csv", clear

summ beta if param == "bmi"
summ beta if param == "covid"


summ beta if param == "bmi_nointeraction"
summ beta if param == "covid_nointeraction"


}


**
** distribution of generated variables


clear all
insheet using "out/sim-`bmiEffect'-`selInteractEffect'0-summaries-all.csv", clear

* should have mean=0, sd=1 i.e. it is standardised
summ mean if variable == "sd_bmi"
summ mean if variable == "sd_bmi_covid"
summ mean if variable == "sd_bmi_nocovid"

* should be ~ 7.20%
summ mean if variable == "covid"

* should be ~ 19.974% selected into tested subsample
summ mean if variable == "selection"
summ mean if variable == "selectionx"

