

# Simulations of SARS-CoV-2 infection based on ALSPAC


## 1. Run simulations

All simulations are run on Blue Crystal phase 3, see job files in the `jobs` directory.



## 2. Check sim generated data

Check the generated data has the intended distributions and relationships.

```bash
Rscript ../combineSummaries.R null 0
Rscript ../combineSummaries.R effect 0
```

```bash
stata mainCheckSims.do
```



## 3. Generate bias statistics


Generate the bias and monte carlo SE for each simulation version:

```bash
Rscript ../sim-bias-estimates.R null 0
Rscript ../sim-bias-estimates.R effect 0
```

```bash
Rscript ../sim-bias-estimates.R null 1
Rscript ../sim-bias-estimates.R effect 1
```


## Plot results:

This needs to be run after generating bias statistics, as that script creates a combined results file for each scenario
(across all blue crytstal job arrays).

```bash
stata -b sim-hists-all.do
```



