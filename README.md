
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tvmedg <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/causalepi/tvmedg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/causalepi/tvmedg/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/causalepi/tvmedg/graph/badge.svg)](https://app.codecov.io/gh/causalepi/tvmedg)

<!-- badges: end -->

The `tvmedg` package implements *causal mediation analysis* using
g-computation in longitudinal settings with time-varying exposure,
mediators, and confounders. It extends the g-computation framework to
decompose total effects into the *randomized interventional analogues*
of *natural direct* and *indirect effects*, in the presence of
time-varying confounding affected by prior exposures or mediators. This
approach builds upon the *‘mediational g-formula’* introduced by
[VanderWeele and Tchetgen Tchetgen
(2017)](https://academic.oup.com/jrsssb/article/79/3/917/7040673).

The current version of `tvmedg` supports multiple mediators, both binary
and continuous exposures, and spline-based functional forms for
continuous variables. The package also enables parallel computing for
efficiency in large-scale analyses. Besides the core modeling functions,
`tvmedg` includes functions for visualizing and diagnosing model
results.

## Installation

The development version of `tvmedg` can be installed from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("causalepi/tvmedg")
```

## Quick Start

``` r
library(tvmedg)
library(doParallel)
#> Loading required package: foreach
#> Loading required package: iterators
#> Loading required package: parallel
```

### Simulation data

``` r
head(sim_data)
#>   id mm Ap Mp L1       L2        L3 Yp      age sex ow risk lastid
#> 1  1  1  0  0  0 100.0000  80.00000  0 16.52949   0  0    0      0
#> 2  1  2  0  0  0 125.1296 102.02885  0 16.52949   0  0    0      0
#> 3  1  3  0  0  0 116.4990  98.99688  0 16.52949   0  0    0      0
#> 4  1  4  0  0  0 131.9247 104.07117  0 16.52949   0  0    0      0
#> 5  1  5  0  0  0 109.5959 103.24813  0 16.52949   0  0    0      0
#> 6  1  6  0  0  0 124.2403  95.82793  0 16.52949   0  0    0      0
```

### Run model

``` r
cl <- makeCluster(8)
registerDoParallel(cl)

op <- tvmedg(data = sim_data,
       id = "id",
       basec = c("age","sex","ow","risk"),
       expo = c("Ap"),
       med = c("Mp"),
       tvar = c("L1","L2","L3"),
       outc = c("Yp"),
       time = c("mm"),
       lag = 2,
       cont_exp = F,
       mreg = "binomial",
       lreg = c("binomial","gaussian","gaussian"),
       yreg = "binomial",
       sp_list = c("mm"),
       sp_type = c("bs"),
       sp_df = c(3),
       followup = 12,
       seed = 123,
       montecarlo = 1000,
       boot = F,
       parallel = TRUE)
#> Q(a,a): 0.222 
#> Q(a,a*): 0.033 
#> Q(a*,a*): 0.014 
#> Indirect (rIE): 0.189 
#> Direct (rDE): 0.019 
#> Total (rTE): 0.208 
#> Proportional explain: 0.909 
#> Total time elapsed: 2.758889 mins

stopCluster(cl)
```
