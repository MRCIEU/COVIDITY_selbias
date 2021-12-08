

***
*** null effect of BMI on covid infection

do ../../sim-hists.do null nointeract
do ../../sim-hists.do null plausible
do ../../sim-hists.do null extreme


***
*** Version with effect of BMI on COVID risk

do ../../sim-hists.do effect nointeract
do ../../sim-hists.do effect plausible
do ../../sim-hists.do effect extreme




