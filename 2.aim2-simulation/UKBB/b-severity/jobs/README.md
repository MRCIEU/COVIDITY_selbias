
# Simulations in UK Biobank - COVID-19 severity



## Main hypothesised DAG


Submit job for simulation with main DAG:

```bash
qsub j-sim-main-null-all.sh
```


## Partial DAGs

Re-run simulation with each subset of selection determinants to investigate what drives the biased estimates.

```bash
qsub j-sim-main-null-bmionly.sh
qsub j-sim-main-null-covarsonly.sh 
qsub j-sim-main-null-covidonly.sh 
qsub j-sim-main-null-severityonly.sh
qsub j-sim-main-null-bmicovid.sh 
qsub j-sim-main-null-bmicovars.sh 
qsub j-sim-main-null-covarscovid.sh 
qsub j-sim-main-null-severitycovid.sh
qsub j-sim-main-null-severitycovars.sh
qsub j-sim-main-null-severitybmi.sh
qsub j-sim-main-null-bmicovarscovid.sh
qsub j-sim-main-null-severitybmicovars.sh
qsub j-sim-main-null-severitybmicovid.sh
qsub j-sim-main-null-severitycovarscovid.sh
```


## Version with effect of BMI on COVID risk

```bash
qsub j-sim-main-effect-all.sh
```

```bash
qsub j-sim-main-effect-bmionly.sh
qsub j-sim-main-effect-covarsonly.sh
qsub j-sim-main-effect-covidonly.sh
qsub j-sim-main-effect-severityonly.sh
qsub j-sim-main-effect-bmicovid.sh
qsub j-sim-main-effect-bmicovars.sh 
qsub j-sim-main-effect-covarscovid.sh 
qsub j-sim-main-effect-severitycovid.sh
qsub j-sim-main-effect-severitycovars.sh
qsub j-sim-main-effect-severitybmi.sh
qsub j-sim-main-effect-bmicovarscovid.sh
qsub j-sim-main-effect-severitybmicovars.sh
qsub j-sim-main-effect-severitybmicovid.sh
qsub j-sim-main-effect-severitycovarscovid.sh
```

## Versions with different OR for the effect of COVID on selection

### Assuming OR=10

```bash
qsub j-sim-main-null-all-OR10.sh
qsub j-sim-main-effect-all-OR10.sh
```

### Assuming OR=5

```bash
qsub j-sim-main-null-all-OR5.sh
qsub j-sim-main-effect-all-OR5.sh
```



