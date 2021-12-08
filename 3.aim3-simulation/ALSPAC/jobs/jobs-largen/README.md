

# Simulations of SARS-CoV-2 infection based on ALSPAC

Sensitivity analysis using sample size of 5million

## Main hypothesised DAG


```bash
qsub j-sim-main-bminull-nointeract.sh
qsub j-sim-main-bminull-plausible.sh
qsub j-sim-main-bminull-extreme.sh
```


## Version with effect of BMI on COVID risk

```bash
qsub j-sim-main-bmieffect-nointeract.sh
qsub j-sim-main-bmieffect-plausible.sh
qsub j-sim-main-bmieffect-extreme.sh
```




