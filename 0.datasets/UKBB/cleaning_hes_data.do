/* 

Alice R Carter - 4/3/21
Cleaning HES data for use in analysis. Scripts adapted from those written by Sean Harrison
in 19278\raw\HES use hes_v2.do 
uses HES data downloaded on 24/02/2020 and cleaned on 4/3/2021

*/

cd "$dataDir\2021-02-24\data"

********************************************************************************
*The diag file needs some mods
import delim "hesin-20210213.txt", clear
sort eid ins_index

save "$resDir\data\COVIDITY\UKBB\ukb_hesin_clean_20210213.dta", replace

********************************************************************************
*Diagnoses data
import delim "hesin_diag-20210213.txt", clear
sort eid ins_index arr_index

drop level *_nb

rename diag_icd9 diag_icd9_
rename diag_icd10 diag_icd10_

reshape wide diag_icd9_ diag_icd10_, i(eid ins_index) j(arr_index)

save  "$resDir\data\COVIDITY\UKBB\ukb_hesin_diag_clean_20210213.dta", replace

********************************************************************************
*Critical care data
import delim "hesin_critical-20210213.txt", clear
sort eid ins_index arr_index

*Remove duplicate participants with the same ID, ins_index and critical care start date

** CHECK THIS - I don't think we do want to do this, this isn't just COVID-19 critical care, so by restricting to first event CC, then we're potentially losing a lot of information

duplicates tag eid ins_index, gen(duplicate)
drop if duplicate >0
capture drop duplicate

save  "$resDir\data\COVIDITY\UKBB\ukb_hesin_critical_clean_20210213.dta", replace

********************************************************************************
cd "$resDir\data\COVIDITY\UKBB"
*Combine
use "ukb_hesin_clean_20210213.dta", clear
merge 1:1 eid ins_index using "ukb_hesin_diag_clean_20210213.dta", nogen
merge 1:1 eid ins_index using "ukb_hesin_critical_clean_20210213.dta"
gen critical_care = 1 if _merge==3
capture drop _merge

gen date_epistart=date(epistart, "YMD", 2050),a(epistart)
format date_epistart %td
drop epistart
rename date_epistart epistart

gen date_epiend=date(epiend, "YMD", 2050),a(epiend)
format date_epiend %td
drop epiend
rename date_epiend epiend

sort eid ins_index

save "hes_20210213_full.dta", replace

keep eid ins_index epistart epiend admidate disdate diag_icd* ccstartdate critical_care

save "hes_20210213.dta", replace
