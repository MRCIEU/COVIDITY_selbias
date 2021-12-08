

***
*** null effect of BMI on covid infection

do ../../sim-hists.do null nointeract 1
do ../../sim-hists.do null plausible 1
do ../../sim-hists.do null extreme 1


***
*** Version with effect of BMI on SARS-CoV-2 infection

do ../../sim-hists.do effect nointeract 1
do ../../sim-hists.do effect plausible 1
do ../../sim-hists.do effect extreme 1





