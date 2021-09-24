
# Simulations in UK Biobank


All simulations are run on Blue Crystal phase 3, see job files in the `jobs` directory.


## Check sim generated data

```bash
stata mainCheckSims.do
```


## Plot results:

```bash
stata -b sim-hists-all.do "out/"
```




## Generate bias statistics


Generate the average bias and monte carlo SE for each simulation version

```bash
Rscript ../../sim-bias-estimates.R null
Rscript ../../sim-bias-estimates.R effect
```
