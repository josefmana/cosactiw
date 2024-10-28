# cosactiw-brief-report

My humble contribution to the COSACTIW vs NANOK comparisons of the primary outcome report approximating
the effect of midlife physical activity on (super)aging parameters.

## Analysis & Summary of Results

The analysis consists of a set of GLMs (logistic for binary variables, least squares Gaussian regressions for continuous variables)
with adjustment sets derived from causal assumptions depicted in the following DAG:

![](DAG.jpg)

Estimates were computed via G-computation of weighted least squares GLMs with propensity scores weights based on logistic
regression of the exposure on the covariates. Results are presented in the following tables for midlife PA, current PA and Education respectively:

![](tab_mPA_causal.jpg)
![](tab_cPA_causal.jpg)
![](tab_Education_causal.jpg)

These can be contrasted against estimates of otherwise equivalent GLMs with no covariates or weighting:

![](tab_mPA_descriptive.jpg)
![](tab_cPA_descriptive.jpg)
![](tab_Education_descriptive.jpg)

## Transparency & Reproducibility

The [renv](https://rstudio.github.io/renv/) package was used to create reproducible environment for the project.
To set-up R environment for reproduction of our results, run:

```
#install.packages("renv")
renv::restore()
```

The [targets](https://docs.ropensci.org/targets/) package was used to create a reproducible analysis pipeline.
The data are not being shared for the time being. If you managed to get them from the main authors, put them into
the `_raw/` folder and run the analyses using the following code:

```
#install.packages("targets")
targets::tar_make()
```

The current pipeline looks as follows:

![](pipeline.jpeg)
