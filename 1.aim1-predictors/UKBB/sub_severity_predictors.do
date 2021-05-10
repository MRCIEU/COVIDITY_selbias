/*

Alice R Carter 13/4/21

Sub-do file to run univariate analyses between UKBB participant characteristics and obtaining a covid test 

*/

local phase = "`1'"
local caseDef = "`2'"
local sampleDef = "`3'"
local date = "`4'"

di "`phase'"
di "`caseDef'"
di "`sampleDef'"
di "`date'"

/* Setting up results file */


	putexcel set `date'_univariate_severity_`sampleDef'_`phase', sheet(results) modify
	
	putexcel A1="Variable" B1="Coef" ///
		C1="Outcome" D1="Coefficient_OR" E1="lower_CI" F1="Upper_CI" G1="N total" H1="N Case" ///
		I1="Outcome" J1="Coefficient_OR" K1="lower_CI" L1="Upper_CI" M1="N total" N1="N Case" ///
		O1="Outcome" P1="Coefficient_OR" Q1="lower_CI" R1="Upper_CI" S1="N total" T1="N Case" ///
		U1="Outcome" V1="Coefficient_OR" W1="lower_CI" X1="Upper_CI" Y1="N total" Z1="N Case"		
		
****************** 		Continuous and binary exposures 		****************** 

local x=1 	
foreach exposure in sd_age sd_bmi sd_sbp sd_dbp sd_tdi sex excess_alcohol ethnicity urban_rural cancer_diag cvd_diag hyperten_diag resp_diag autoimmune_diag mh_diag {
  local i = 2
  local x=`x'+1
	foreach outcome in death_`caseDef'nonsevere_`phase' death_`caseDef'tested_`phase' death_`caseDef'negative_`phase' death_`caseDef'population_`phase' {
   
	putexcel set `date'_univariate_severity_`sampleDef'_`phase', sheet(results) modify
	
	logistic `outcome' `exposure'
	
	matrix results = r(table)
	local beta = results[1,1]
	local lci = results[5,1]
	local uci = results[6,1]
	local p_value = results[4,1]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)

	putexcel A`x'="`exp_label'" B`x'="`exp_label'" 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'
	putexcel `letter'`x'="`out_label'" 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`beta' 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`lci' 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`uci' 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`n' 
	
	tab `outcome' if `exposure'!=. , matcell(numbers)	
		local case = numbers[2,1]
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`case' 	
	}
}

****************** 		Current smoking categorical 		****************** 

local x=17
local y=18
foreach exposure in current_smoke  {
  local i = 2
	foreach outcome in death_`caseDef'nonsevere_`phase' death_`caseDef'tested_`phase' death_`caseDef'negative_`phase' death_`caseDef'population_`phase' {
   
	putexcel set `date'_univariate_severity_`sampleDef'_`phase', sheet(results) modify
    
	logistic `outcome' i.`exposure'

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
	
	putexcel A`x'="`exp_label'" B`x'="Former smoker" A`y'="`exp_label'" B`y'="Current smoker" 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'
	putexcel `letter'`x'="`out_label'" `letter'`y'="`out_label'"  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`beta_1' `letter'`y'=`beta_2'  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`lci_1' `letter'`y'=`lci_2'  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`uci_1' `letter'`y'=`uci_2'  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`n' `letter'`y'=`n' 
	
	tab `outcome' if `exposure'!=. , matcell(numbers)	
		local case = numbers[2,1]
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`case' `letter'`y'=`case' 
	
	}
}

****************** 		Education categorical exposure		****************** 

local x=19
local y=20
local z=21
foreach exposure in eduyears_quali  {
  local i = 2	
	
	foreach outcome in death_`caseDef'nonsevere_`phase' death_`caseDef'tested_`phase' death_`caseDef'negative_`phase' death_`caseDef'population_`phase' {
   
	putexcel set `date'_univariate_severity_`sampleDef'_`phase', sheet(results) modify
    
	logistic `outcome' i.`exposure' if `outcome'!=.

	matrix results = r(table)
	local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
	local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,4]
	local beta_3 = results[1,4]
	local lci_3 = results[5,4]
	local uci_3 = results[6,4]
	local p_value_3 = results[4,4]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`exp_label'" B`x'="AS/A level" A`y'="`exp_label'" B`y'="NVQ/vocational quali" A`z'="`exp_label'" B`z'="Degree or higher"  	
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'
	putexcel `letter'`x'="`out_label'" `letter'`y'="`out_label'"  `letter'`z'="`out_label'"  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`beta_1' `letter'`y'=`beta_2' `letter'`z'=`beta_3'  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`lci_1' `letter'`y'=`lci_2' `letter'`z'=`lci_3'
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`uci_1' `letter'`y'=`uci_2' `letter'`z'=`uci_3'
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`n' `letter'`y'=`n' `letter'`z'=`n' 
	
	tab `outcome' if `exposure'!=. , matcell(numbers)	
		local case = numbers[2,1]
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`case' `letter'`y'=`case' `letter'`z'=`case'
	
	}
}

****************** 			TDI categorical exposure		****************** 

local x=22
local y=23
local z=24
local a=25
foreach exposure in tdi_cat  {
	local i=2
	
	foreach outcome in death_`caseDef'nonsevere_`phase' death_`caseDef'tested_`phase' death_`caseDef'negative_`phase' death_`caseDef'population_`phase' {
   
	putexcel set `date'_univariate_severity_`sampleDef'_`phase', sheet(results) modify
    
	logistic `outcome' i.`exposure' 

	matrix results = r(table)
		local beta_1 = results[1,2]
	local lci_1 = results[5,2]
	local uci_1 = results[6,2]
	local p_value_1 = results[4,2]
		local beta_2 = results[1,3]
	local lci_2 = results[5,3]
	local uci_2 = results[6,3]
	local p_value_2 = results[4,4]
		local beta_3 = results[1,4]
	local lci_3 = results[5,4]
	local uci_3 = results[6,4]
	local p_value_3 = results[4,4]
		local beta_4 = results[1,5]
	local lci_4 = results[5,5]
	local uci_4 = results[6,5]
	local p_value_4 = results[4,5]
	local exp_label : var label `exposure'
	local out_label : var label `outcome'
	local n = e(N)
	
	putexcel A`x'="`exp_label'" B`x'="TDI 2" A`y'="`exp_label'" B`y'="TDI 3" A`z'="`exp_label'" B`z'="TDI 4" A`a'="`exp_label'" B`a'="TDI 5"   
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'
	putexcel `letter'`x'="`out_label'" `letter'`y'="`out_label'" `letter'`z'="`out_label'" `letter'`a'="`out_label'"    
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`beta_1' `letter'`y'=`beta_2' `letter'`z'=`beta_3' `letter'`a'=`beta_4' 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`lci_1' `letter'`y'=`lci_2' `letter'`z'=`lci_3' `letter'`a'=`lci_4'  
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`uci_1' `letter'`y'=`uci_2' `letter'`z'=`uci_3' `letter'`a'=`uci_4' 
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`n' `letter'`y'=`n' `letter'`z'=`n' `letter'`a'=`n'    	

	tab `outcome' if `exposure'!=. , matcell(numbers)	
		local case = numbers[2,1]
		local i=`i'+1
		local letter: word `i' of `c(ALPHA)'	
	putexcel `letter'`x'=`case' `letter'`y'=`case' `letter'`z'=`case' `letter'`a'=`case'	 

	}
}



