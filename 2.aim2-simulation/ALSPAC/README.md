

# Simulations of SARS-CoV-2 infection based on ALSPAC


## Run simulations

All simulations are run on Blue Crystal phase 3, see job files in the `jobs` directory.



## Plot results:

```bash
stata -b sim-hists-all.do
```



## Generate bias statistics


Generate the average bias and monte carlo SE for each simulation version

```bash
Rscript ../sim-bias-estimates.R null "infection"
Rscript ../sim-bias-estimates.R effect "infection"
```

