
# Simulations in UK Biobank - COVID-19 severity


All simulations are run on Blue Crystal phase 3, see job files in the `jobs` directory.



## Plot results:

```bash
stata -b sim-hists-all.do "out/"
```




## Generate bias statistics


Generate the average bias and monte carlo SE for each simulation version

```bash
Rscript ../../sim-bias-estimates.R "null" "severity"
Rscript ../../sim-bias-estimates.R "effect" "severity"
```
