

***
*** null effect of BMI on covid infection

* all pathways affecting selection

do ../sim-hists.do null all 2

* Partial DAGs

do ../sim-hists.do null bmi
do ../sim-hists.do null covars
do ../sim-hists.do null covid
do ../sim-hists.do null bmi_covars
do ../sim-hists.do null bmi_covid
do ../sim-hists.do null covars_covid


***
*** Version with effect of BMI on COVID risk

do ../sim-hists.do effect all 2


do ../sim-hists.do effect bmi
do ../sim-hists.do effect covars
do ../sim-hists.do effect covid
do ../sim-hists.do effect bmi_covars
do ../sim-hists.do effect bmi_covid
do ../sim-hists.do effect covars_covid



***
*** Extreme versions with different OR for the effect of COVID on selection

* Assuming OR=10


do ../sim-hists.do null all 10
do ../sim-hists.do effect all 10

* Assuming OR=5

do ../sim-hists.do null all 5
do ../sim-hists.do effect all 5




