

***
*** null effect of BMI on covid infection

* all pathways affecting selection

do ../../sim-hists.do null all 2

* Partial DAGs

do ../../sim-hists.do null bmi 2
do ../../sim-hists.do null covars 2
do ../../sim-hists.do null covid 2
do ../../sim-hists.do null severity 2
do ../../sim-hists.do null bmi_covars 2
do ../../sim-hists.do null bmi_covid 2
do ../../sim-hists.do null covars_covid 2
do ../../sim-hists.do null severity_bmi 2
do ../../sim-hists.do null severity_covars 2
do ../../sim-hists.do null severity_covid 2
do ../../sim-hists.do null severity_bmi_covars 2
do ../../sim-hists.do null severity_bmi_covid 2
do ../../sim-hists.do null severity_covars_covid 2
do ../../sim-hists.do null bmi_covars_covid 2

***
*** Version with effect of BMI on COVID risk

do ../../sim-hists.do effect all 2


do ../../sim-hists.do effect bmi 2
do ../../sim-hists.do effect covars 2
do ../../sim-hists.do effect covid 2
do ../../sim-hists.do effect severity 2
do ../../sim-hists.do effect bmi_covars 2
do ../../sim-hists.do effect bmi_covid 2
do ../../sim-hists.do effect covars_covid 2
do ../../sim-hists.do effect severity_bmi 2
do ../../sim-hists.do effect severity_covars 2
do ../../sim-hists.do effect severity_covid 2
do ../../sim-hists.do effect severity_bmi_covars 2
do ../../sim-hists.do effect severity_bmi_covid 2
do ../../sim-hists.do effect severity_covars_covid 2
do ../../sim-hists.do effect bmi_covars_covid 2


***
*** Extreme versions with different OR for the effect of COVID on selection

* Assuming OR=10


do ../../sim-hists.do null all 10
do ../../sim-hists.do effect all 10

* Assuming OR=5

do ../../sim-hists.do null all 5
do ../../sim-hists.do effect all 5




