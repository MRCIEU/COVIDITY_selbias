

*** Louise AC Millard August 2021
***

* iteration number
local i = `1'

* covariate names
local covars = "`2'"



* using selection generated with an interaction

poisson selection sd_bmi covid `covars' 
*poisson selection sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi covid
local beta _b[sd_bmi]
local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
file write myfile3 %04.0f (`i') ",bmi," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n
local beta _b[covid]
local ciL _b[covid] - 1.96 * _se[covid]
local ciU _b[covid] + 1.96 * _se[covid]                
file write myfile3 %04.0f (`i') ",covid," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n



* using selectionx generated without an interaction (i.e. main effects only)

poisson selectionx sd_bmi covid `covars'
*poisson selectionx sd_bmi education_alevel education_voc education_degree sex_m sd_age smoking_previous smoking_current sd_tdi covid
local beta _b[sd_bmi]
local ciL _b[sd_bmi] - 1.96 * _se[sd_bmi]
local ciU _b[sd_bmi] + 1.96 * _se[sd_bmi]
file write myfile3 %04.0f (`i') ",bmi_nointeraction," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n
local beta _b[covid]
local ciL _b[covid] - 1.96 * _se[covid]
local ciU _b[covid] + 1.96 * _se[covid]
file write myfile3 %04.0f (`i') ",covid_nointeraction," %7.6f (`beta') "," %7.6f (`ciL') "," %7.6f (`ciU') _n


