
# Simulations in UK Biobank


All simulations are run on Blue Crystal phase 3, see job files in the `jobs` directory.


## Main hypothesised DAG


Plot results:

```bash
stata -b ../../sim-hists.do null all
```


## Partial DAGs

Re-run simulation with each subset of selection determinants to investigate what drives the biased estimates.


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
stata -b ../../sim-hists.do effect all
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
stata -b ../../sim-hists.do null all 10
stata -b ../../sim-hists.do effect all 10
```

### Assuming OR=5

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

