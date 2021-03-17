/* 

Alice R. Carter - 04/03/2021
This script codes up cases according to ICD9 and ICD10 code from the HES released data (cleaned in "cleaning_hes_data.do"). This data was released on the 24/02/2021 and cleaned on the 04/03/2021. Diagnoses dervied here are the same as those in the main data set 

The cases defined in this script are used in analyses as comorbidities considered as predictors of selection

*/

cd "$resDir\data\COVIDITY\UKBB"

use "hes_20210213.dta", clear

rename diag_icd* icd*

/* Generate diagnosis variables */
generate cancer_diag = 0
lab var cancer_diag "Diagnosis - Cancer (ref = no cancer)"
generate cvd_diag = 0
lab var cvd_diag "Diagnosis - Cardiovascular (ref = no CVD)"
generate hyperten_diag = 0
lab var hyperten_diag "Diagnosis - Hypertension (ref = no HTN)"
generate diabetes_diag = 0
lab var diabetes_diag "Diagnosis - Diabetes (ref = no diab)"
generate resp_diag = 0
lab var resp_diag "Diagnosis - Respiratory (ref = no resp dis)"
generate autoimmune_diag = 0
lab var autoimmune_diag "Diagnosis - Autoimmune disease (ref = no autoimmune)"
generate mh_diag = 0
lab var mh_diag "Diagnosis - Mental health (ref = no MH diag)"
generate covid_diag = 0
lab var covid_diag "Diagnosis - Covid inpatient (ref = no hospital covid)"
label define casecontrol 0 "Control" 1 "Case", modify
label values *_diag casecontrol

/* Code ICD 10 diagnoses */

global diagnoses cancer_diag cvd_diag hyperten_diag diabetes_diag resp_diag autoimmune_diag mh_diag covid_diag

foreach cause of varlist $diagnoses {
    generate cancer_code_`cause' =.
	generate cvd_code_`cause' =.
	generate hyperten_code_`cause' =.
	generate diabetes_code_`cause' =.
	generate resp_code_`cause' =.
	generate autoimmune_code_`cause' =.
	generate mh_code_`cause' =.
	generate covid_code_`cause' =.

	}
	
* ICD 10 codes
foreach diag of varlist icd10_* {
foreach cause of varlist $diagnoses {
	*generate variable to capture ICD codes relating to cancer ICD-C*)
    replace cancer_code_`cause' = strpos(`diag', "C0") > 0 
    replace cancer_code_`cause' = strpos(`diag', "C1") > 0 if cancer_code_`cause' == 0
    replace cancer_code_`cause' = strpos(`diag', "C2") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C3") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C4") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C5") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C6") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C7") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C8") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "C9") > 0 if cancer_code_`cause' == 0
	replace cancer_code_`cause' = strpos(`diag', "D0") > 0 if cancer_code_`cause' == 0
	
	* generate variable to capture ICD codes relating to CVD (ICD-I*)
	replace cvd_code_`cause' = strpos(`diag', "I") > 0 

	* generate variable to capture ICD codes relating to AMI (ICDI21*)	
	replace hyperten_code_`cause' = strpos(`diag', "I10") > 0 
	replace hyperten_code_`cause' = strpos(`diag', "I11") > 0  if hyperten_code_`cause' == 0
	replace hyperten_code_`cause' = strpos(`diag', "I12") > 0  if hyperten_code_`cause' == 0
	replace hyperten_code_`cause' = strpos(`diag', "I13") > 0  if hyperten_code_`cause' == 0
	replace hyperten_code_`cause' = strpos(`diag', "I14") > 0  if hyperten_code_`cause' == 0
	replace hyperten_code_`cause' = strpos(`diag', "I15") > 0  if hyperten_code_`cause' == 0

	* generate variable to capture ICD codes relating to diabetes (E11-E14) Note: This doesn't include type 1
	* T1D (E10) is included in autoimmune diseases
	replace diabetes_code_`cause' = strpos(`diag', "E11") > 0 
	replace diabetes_code_`cause' = strpos(`diag', "E12") > 0 if diabetes_code_`cause' == 0
	replace diabetes_code_`cause' = strpos(`diag', "E13") > 0 if diabetes_code_`cause' == 0
	replace diabetes_code_`cause' = strpos(`diag', "E14") > 0 if diabetes_code_`cause' == 0
	
	* generate variable to capture ICD codes relating to respiratory disease (J*))
	replace resp_code_`cause' = strpos(`diag', "J3") > 0 
	replace resp_code_`cause' = strpos(`diag', "J4") > 0 if resp_code_`cause' == 0
	replace resp_code_`cause' = strpos(`diag', "J6") > 0 if resp_code_`cause' == 0
	replace resp_code_`cause' = strpos(`diag', "J70") > 0 if resp_code_`cause' == 0
	replace resp_code_`cause' = strpos(`diag', "J8") > 0 if resp_code_`cause' == 0
	replace resp_code_`cause' = strpos(`diag', "J8") > 0 if resp_code_`cause' == 0
	replace resp_code_`cause' = strpos(`diag', "J9") > 0 if resp_code_`cause' == 0

	* generate variable to capture ICD codes relating to autoimmune disease 
	replace autoimmune_code_`cause' = strpos(`diag', "D68.3") > 0 
	replace autoimmune_code_`cause' = strpos(`diag', "D68.4") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "D68.5") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "D69.3") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "D86") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "D89") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "E06.3") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "E10") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "E27.1") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "E27.2") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "G35") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "G61") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "G35") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "G61") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "G70") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "H46") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "I77.6") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K50") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K51") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K74.3") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K75.4") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K83.01") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "K90.0") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L10") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L11") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L12") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L13") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L14") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L40") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L43") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L63.9") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L80") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "L90.0") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M05") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M06") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M07") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M08") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M30") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M31") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M32") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M33") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M34") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M35") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M45") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.0") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.1") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.2") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.4") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.8") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "M46.9") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "N08") > 0 if autoimmune_code_`cause' == 0
	replace autoimmune_code_`cause' = strpos(`diag', "D51.0") > 0 if autoimmune_code_`cause' == 0

	* generate variable to capture ICD codes relating to mental health diagnoses
	replace mh_code_`cause' = strpos(`diag', "F32") > 0 
	replace mh_code_`cause' = strpos(`diag', "F33") > 0 if mh_code_`cause' == 0
	replace mh_code_`cause' = strpos(`diag', "F41") > 0 if mh_code_`cause' == 0

	* generate variable to capture ICD codes relating to covid 
	replace covid_code_`cause' = strpos(`diag', "U07") > 0 
	
	* update case status
	replace cancer_diag = 1 if cancer_code_`cause' == 1
	replace cvd_diag = 1 if cvd_code_`cause' == 1
	replace hyperten_diag = 1 if hyperten_code_`cause' == 1
	replace diabetes_diag = 1 if diabetes_code_`cause' == 1
	replace resp_diag = 1 if resp_code_`cause' == 1
	replace autoimmune_diag = 1 if autoimmune_code_`cause' == 1 
	replace mh_diag = 1 if mh_code_`cause' ==1
	replace covid_diag = 1 if covid_code_`cause' ==1
	
	}
}
drop *_code_*

save "hes_diagnoses.dta", replace

/* Code ICD-9 diagnoses */

*ICD 9 codes

* generate variable to capture ICD codes relating to Cancer
gen byte cancer_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 140/165 {
        replace  cancer_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 

foreach var of varlist icd9_* {
    forvalues i = 170/176 {
        replace  cancer_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 								
	
foreach var of varlist icd9_* {
    forvalues i = 179/209 {
        replace  cancer_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 	
	
foreach var of varlist icd9_* {
    forvalues i = 230/239 {
        replace  cancer_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 		
		               
* generate variable to capture ICD codes relating to CVD (ICDI*)
gen byte cvd_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 390/459 {
        replace cvd_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 
		
* generate variable to capture ICD codes relating to hypertension          
gen byte hyperten_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 401/405 {
        replace hyperten_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
	}

										
* generate variable to capture ICD codes relating to diabetes (ICD 250*)
gen byte diabetes_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 2500/2509 {
        replace diabetes_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 	

* generate variable to capture ICD codes relating to respiratory disorders         
gen byte resp_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 470/478 {
        replace resp_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 	

foreach var of varlist icd9_* {
    forvalues i = 480/488 {
        replace resp_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 

foreach var of varlist icd9_* {
    forvalues i = 490/496 {
        replace resp_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 

foreach var of varlist icd9_* {
    forvalues i = 500/508 {
        replace resp_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 

foreach var of varlist icd9_* {
    forvalues i = 510/519 {
        replace resp_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 

* generate variable to capture ICD codes relating to autoimmune disorders         
gen byte autoimmune_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 2865/2865 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 2867/2867 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 28981/28981 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 28731/28731 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 135/135 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 2452/2452 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 250/250 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 25541/25541 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 340/340 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 35781/35781 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 358/358 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 3773/3773 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 4776/4776 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 555/556 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 5716/5716 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 57142/57142 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 5761/5761 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 5790/5790 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 694/694 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 696/6970 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 70401/70401 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 70901/70901 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 7010/7010 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 714/714 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 446/446 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 710/710 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 7200/7200 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 7208/7208 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

foreach var of varlist icd9_* {
    forvalues i = 2810/2810 {
        replace autoimmune_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
}	

* generate variable to capture ICD codes relating to adverse mental health         
gen byte mh_code_ICD9 = 0
foreach var of varlist icd9_* {
    forvalues i = 296/296 {
        replace mh_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 	

foreach var of varlist icd9_* {
    forvalues i = 300/300 {
        replace mh_code_ICD9 = 1 if substr(`var',1,3) == "`i'"
    }
} 
												
replace cancer_diag = 1 if cancer_code_ICD9 ==1
replace cvd_diag = 1 if cvd_code_ICD9 ==1
replace hyperten_diag = 1 if hyperten_code_ICD9 ==1
replace diabetes_diag = 1 if diabetes_code_ICD9 ==1
replace resp_diag 	= 1 if resp_code_ICD9 ==1
replace autoimmune_diag = 1 if autoimmune_code_ICD9 ==1
replace mh_diag = 1 if mh_code_ICD9 ==1

save "hes_diagnoses.dta", replace

* Collapse down duplicate IDs to single row per participant

/* Note: epistart hasn't copied over proeprly from the raw data so is missing in all. Need to check in the raw HES data why this is, but shouldn't cause any issues for the time being */
gen date = epistart
replace date = epiend if date==.
replace date = date(admidate, "YMD") if date==.
format date %td

global diagnoses cancer_diag cvd_diag hyperten_diag diabetes_diag resp_diag autoimmune_diag mh_diag covid_diag

local x=0
foreach var of varlist $diagnoses { 

	local x = `x'+1
	gen v`x' = `var'
	lab var v`x' `var'
	replace v`x'=0 if v`x'==.

}

local icd_total=8

forvalues i = 1/`icd_total' {
	bysort eid: egen x = max(v`i')
	replace v`i' = x 

	drop x
}

duplicates tag eid, gen(duplicate)

duplicates drop eid, force
egen count = rowtotal(v*)
drop if count == 0 
drop count

drop *_diag

rename v1 cancer_diag_hes
rename v2 cvd_diag_hes
rename v3 hyperten_diag_hes
rename v4 diabetes_diag_hes
rename v5 resp_diag_hes
rename v6 autoimmune_diag_hes
rename v7 mh_diag_hes
rename v8 covid_diag_hes

save "hes_diagnoses_20210213.dta", replace
