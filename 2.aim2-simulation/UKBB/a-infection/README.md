
# Simulations in UK Biobank



## Main hypothesised DAG


Submit job for simulation with main DAG:

```bash
qsub j-sim-main-null-all.sh
```

Plot results:

```bash
stata -b ../../sim-hists.do null all
```


## Partial DAGs

Re-run simulation with each subset of selection determinants to investigate what drives the biased estimates.

```bash
qsub j-sim-main-null-bmionly.sh
qsub j-sim-main-null-covarsonly.sh 
qsub j-sim-main-null-covidonly.sh 
qsub j-sim-main-null-bmicovid.sh 
qsub j-sim-main-null-bmicovars.sh 
qsub j-sim-main-null-covarscovid.sh 
```

Plot results:

```bash
stata -b ../../sim-hists.do null bmi
stata -b ../../sim-hists.do null covars
stata -b ../../sim-hists.do null covid
stata -b ../../sim-hists.do null bmi_covars
stata -b ../../sim-hists.do null bmi_covid
stata -b ../../sim-hists.do null covars_covid
```


## Version with effect of BMI on COVID risk

```bash
qsub j-sim-main-effect-all.sh
```

```bash
stata -b ../../sim-hists.do effect all
```

```bash
qsub j-sim-main-effect-bmionly.sh
qsub j-sim-main-effect-covarsonly.sh
qsub j-sim-main-effect-covidonly.sh
qsub j-sim-main-effect-bmicovid.sh
qsub j-sim-main-effect-bmicovars.sh 
qsub j-sim-main-effect-covarscovid.sh 
```

Plot results:

```bash
stata -b ../../sim-hists.do effect bmi
stata -b ../../sim-hists.do effect covars
stata -b ../../sim-hists.do effect covid
stata -b ../../sim-hists.do effect bmi_covars
stata -b ../../sim-hists.do effect bmi_covid
stata -b ../../sim-hists.do effect covars_covid
```




## Versions with different OR for the effect of COVID on selection

### Assuming OR=10

```bash
qsub j-sim-main-null-all-OR10.sh
qsub j-sim-main-effect-all-OR10.sh
```

```bash
stata -b ../../sim-hists.do null all 10
stata -b ../../sim-hists.do effect all 10
```

### Assuming OR=5

```bash
qsub j-sim-main-null-all-OR5.sh
qsub j-sim-main-effect-all-OR5.sh
```

```bash
stata -b ../../sim-hists.do null all 5
stata -b ../../sim-hists.do effect all 5
```



###
### Bias statistics


Generate the average bias and monte carlo SE for each simulation version

```bash
Rscript ../../sim-bias-estimates.R null
Rscript ../../sim-bias-estimates.R effect
```
