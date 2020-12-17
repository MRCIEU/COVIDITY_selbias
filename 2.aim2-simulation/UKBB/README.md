
# Simulations in UK Biobank



## Main hypothesised DAG


Submit job for simulation with main DAG:

```bash
qsub j-sim-main-null-all.sh
```

Plot results:

```bash
stata -b sim-hists.do null all
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
stata -b sim-hists.do null bmi
stata -b sim-hists.do null covars
stata -b sim-hists.do null covid
stata -b sim-hists.do null bmi_covars
stata -b sim-hists.do null bmi_covid
stata -b sim-hists.do null covars_covid
```


## Version with effect of BMI on COVID risk

```bash
qsub j-sim-main-effect-all.sh
```

```bash
stata -b sim-hists.do effect all
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
stata -b sim-hists.do effect bmi
stata -b sim-hists.do effect covars
stata -b sim-hists.do effect covid
stata -b sim-hists.do effect bmi_covars
stata -b sim-hists.do effect bmi_covid
stata -b sim-hists.do effect covars_covid
```



## Extreme version

To show bias does occur if relationships are strong enough

```bash
qsub j-sim-main-null-all-extreme.sh
```

Plot results:

```bash
stata -b sim-hists.do null all-extreme
```
