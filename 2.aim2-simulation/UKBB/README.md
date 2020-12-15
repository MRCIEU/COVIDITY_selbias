
# Simulations in UK Biobank



## Main hypothesised DAG


Submit job for simulation with main DAG:

```bash
qsub j-sim-main-null.sh
```

Plot results:

```bash
stata -b sim-hists.do all
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
stata -b sim-hists.do bmi
stata -b sim-hists.do covars
stata -b sim-hists.do covid
stata -b sim-hists.do bmi_covars
stata -b sim-hists.do bmi_covid
stata -b sim-hists.do covars_covid
```

