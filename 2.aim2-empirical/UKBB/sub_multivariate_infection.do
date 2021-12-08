/*

Alice R Carter 14/4/21

Sub-do file to run univariate analyses between UKBB participant characteristics and testing positive for COVID-19 infection

The three exposures of interest are: sex, BMI and smoking
Covariates of interest are:age, sex, smoking and SEP (individual level education and neighbourhood level TDI)

*/

*set macros from Args
local phase = "`1'"
local caseDef = "`2'"
local sampleDef = "`3'"
local date = "`4'"

di "`phase'"
di "`caseDef'"
di "`sampleDef'"
di "`date'"

/* Setting up results file */

foreach exposure in sex sd_bmi current_smoke {

	putexcel set `date'_multivariate_infection_`exposure'_`sampleDef'_`phase', sheet(`exposure') modify
	
	putexcel A1="Outcome" B1="Variable" C1="Levels" D1="N" ///
		E1="Coefficient_OR" F1="lower_CI" G1="Upper_CI" H1="P value" I1 = "Model adjustment"

}

* Association between BMI and COVID-19 outcomes relating to testing and infection, sequentially adjusting for confounders

foreach exposure in sd_bmi {
  local x=1 
	foreach outcome in `caseDef'_`phase' positive_`caseDef'_nontested_`phase' negative_`caseDef'_nontested_`phase' positive_`caseDef'_negative_`phase' positive_`caseDef'_pop_`phase' {
    
	local x=`x'+1
	
	putexcel set `date'_multivariate_infection_`exposure'_`sampleDef'_`phase', sheet(`exposure') modify
	
	logistic `outcome' `exposure' 
	
	matrix results = r(table)
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="All UKBB" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value' I`x'="Unadjusted"

	logistic `outcome' `exposure' sd_age sex
	
	local x = `x'+1
	matrix results = r(table)
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="All UKBB" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value' I`x'="Age and sex"	
	
	
	logistic `outcome' `exposure' sd_age sex current_smoke
	
	local x = `x'+1
	matrix results = r(table)
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="All UKBB" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value' I`x'="Age, sex and smoking"	
	
	logistic `outcome' `exposure' sd_age sex current_smoke tdi_cat eduyears_quali
	
	local x=`x'+1
	matrix results = r(table)
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="All UKBB" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci' H`x'=`p_value' I`x'="Age, sex, smoking, SEP"
	}
}

* Association between sex and COVID-19 outcomes
foreach exposure in sex  {
local x=1   
	foreach outcome in `caseDef'_`phase' positive_`caseDef'_nontested_`phase' negative_`caseDef'_nontested_`phase' positive_`caseDef'_negative_`phase' positive_`caseDef'_pop_`phase' {
    
	local x=`x'+1
	
	putexcel set `date'_multivariate_infection_`exposure'_`sampleDef'_`phase', sheet(`exposure') modify
    
	logistic `outcome' i.`exposure' if `outcome'!=.

	matrix results = r(table)
	local beta = results[1,2]
	local lci = results[5,2]
	local uci = results[6,2]
	local p_value = results[4,2]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Female (Baseline)" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci'  H`x'=`p_value' I`x'="Unadjusted"

	logistic `outcome' i.`exposure' sd_age  if `outcome'!=.

	local x=`x'+1
	matrix results = r(table)
	local beta = results[1,2]
	local lci = results[5,2]
	local uci = results[6,2]
	local p_value = results[4,2]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Female (Baseline)" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci'  H`x'=`p_value' I`x'="Age"

	logistic `outcome' i.`exposure' sd_age sd_bmi current_smoke  if `outcome'!=.

	local x=`x'+1
	matrix results = r(table)
	local beta = results[1,2]
	local lci = results[5,2]
	local uci = results[6,2]
	local p_value = results[4,2]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Female (Baseline)" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci'  H`x'=`p_value' I`x'="Age, BMI and smoking"

	logistic `outcome' i.`exposure' sd_age sd_bmi current_smoke eduyears_quali tdi_cat if `outcome'!=.

	local x=`x'+1
	matrix results = r(table)
	local beta = results[1,2]
	local lci = results[5,2]
	local uci = results[6,2]
	local p_value = results[4,2]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Female (Baseline)" D`x'=`n' E`x'=`beta' F`x'=`lci' G`x'=`uci'  H`x'=`p_value' I`x'="Age, BMI, smoking and SEP"

	}
}

* Association between smoking and COVID-19 outcomes
local x=1

foreach exposure in  current_smoke  {

	foreach outcome in `caseDef'_`phase' positive_`caseDef'_nontested_`phase' negative_`caseDef'_nontested_`phase' positive_`caseDef'_negative_`phase' positive_`caseDef'_pop_`phase' {
    
	local x=`x'+1
	
	putexcel set `date'_multivariate_infection_`exposure'_`sampleDef'_`phase', sheet(`exposure') modify
    
	logistic `outcome' i.`exposure' if `outcome'!=.

	matrix results = r(table)
	local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
	local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,3]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Former smoker" D`x'=`n' D`x'=`n' E`x'=`beta_1' F`x'=`lci_1' G`x'=`uci_1' H`x'=`p_value_1' I`x'="Unadjusted"
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Current smoker" D`x'=`n'  E`x'=`beta_2' F`x'=`lci_2' G`x'=`uci_2' H`x'=`p_value_2' I`x'="Unadjusted"
	
	logistic `outcome' i.`exposure' sex sd_age if `outcome'!=.

	matrix results = r(table)
	local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
	local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,3]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Former smoker" D`x'=`n' D`x'=`n' E`x'=`beta_1' F`x'=`lci_1' G`x'=`uci_1' H`x'=`p_value_1' I`x'="Age and sex"
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Current smoker" D`x'=`n'  E`x'=`beta_2' F`x'=`lci_2' G`x'=`uci_2' H`x'=`p_value_2' I`x'="Age and sex"
	
	logistic `outcome' i.`exposure' sex sd_age sd_bmi if `outcome'!=.

	matrix results = r(table)
	local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
	local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,3]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Former smoker" D`x'=`n' D`x'=`n' E`x'=`beta_1' F`x'=`lci_1' G`x'=`uci_1' H`x'=`p_value_1' I`x'="Age, sex and BMI"
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Current smoker" D`x'=`n'  E`x'=`beta_2' F`x'=`lci_2' G`x'=`uci_2' H`x'=`p_value_2' I`x'="Age, sex and BMI"
	
	logistic `outcome' i.`exposure' sex sd_age sd_bmi eduyears_quali tdi_cat if `outcome'!=.

	matrix results = r(table)
	local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
	local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,3]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Former smoker" D`x'=`n' D`x'=`n' E`x'=`beta_1' F`x'=`lci_1' G`x'=`uci_1' H`x'=`p_value_1' I`x'="Age, sex, BMI and SEP"
	local x=`x'+1
	putexcel A`x'="`out_label'" B`x'="`exp_label'" C`x'="Current smoker" D`x'=`n'  E`x'=`beta_2' F`x'=`lci_2' G`x'=`uci_2' H`x'=`p_value_2' I`x'="Age, sex, BMI and SEP"
	}
}
