
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glmmboot

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/colmanhumphrey/glmmboot.svg?branch=master)](https://travis-ci.org/colmanhumphrey/glmmboot)
[![Codecov test
coverage](https://codecov.io/gh/colmanhumphrey/glmmboot/branch/master/graph/badge.svg)](https://codecov.io/gh/colmanhumphrey/glmmboot?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/glmmboot)](https://cran.r-project.org/package=glmmboot)
<!-- badges: end -->

## Overview

glmmboot provides a simple interface for creating bootstrap confidence
intervals using a wide set of models. The primary function is
`bootstrap_model`, which has three primary arguments:

  - `base_model`: the model run on the full data as you normally would,
    prior to bootstrapping
  - `base_data`: the dataset used
  - `resamples`: how many bootstrap resamples you wish to perform

Another function, `bootstrap_ci`, converts output from bootstrap model
runs into confidence intervals and p-values. By default,
`bootstrap_model` calls `bootstrap_ci`.

## Types of bootstrapping

For models with random effects:

  - the default (and recommended) behaviour will be to block sample over
    the effect with the largest entropy (generally the one with the most
    levels)
  - it’s also possible to specify multiple random effects to block
    sample over

With no random effects, performs case resampling: resamples each row
with replacement.

## Requirements:

1.  the model should work with the function `update`, to change the data
2.  the coefficients are extractable using `coef(summary(model))`

<!-- end list -->

  - either directly, i.e. this gives a matrix
  - or it’s a list of matrices; this includes e.g. zero-inflated models,
    which produce two matrices of coefficients

## Parallel

It may be desired to run this package in parallel. The best way is to
use the `future` backend, which uses `future.apply::future_lapply`. You
do that by specifying the backend through the `future::plan` setup, and
then setting `parallelism = "future"`. See the Quick Use vignette for
more.

It’s also easy to use `parallel::mclapply`; again, see the Quick Use
vignette.

## Installation

glmmboot is on CRAN, so you can install it normally:

``` r
install.packages("glmmboot")
```

Or the development version:

``` r
## install.packages("devtools")
devtools::install_github("ColmanHumphrey/glmmboot")
```

## Example: glm (no random effect)

We’ll provide a quick example using glm. First we’ll set up some data:

``` r
x1 <- rnorm(50)
x2 <- runif(50)

expit <- function(x){exp(x) / (1 + exp(x))}

y_mean <- expit(0.2 - 0.3 * x1 + 0.4 * x2)

y <- rbinom(50, 1, prob = y_mean)

sample_frame <- data.frame(x1 = x1, x2 = x2, y = y)
```

Typically this model is fit with logistic regression:

``` r
base_run <- glm(y ~ x1 + x2, 
                family = binomial(link = 'logit'), 
                data = sample_frame)
summary(base_run)
# 
# Call:
# glm(formula = y ~ x1 + x2, family = binomial(link = "logit"), 
#     data = sample_frame)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.5543  -1.2859   0.8632   0.9769   1.2337  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)   0.3403     0.5334   0.638    0.523
# x1           -0.2684     0.2883  -0.931    0.352
# x2            0.2708     0.9480   0.286    0.775
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 66.406  on 49  degrees of freedom
# Residual deviance: 65.414  on 47  degrees of freedom
# AIC: 71.414
# 
# Number of Fisher Scoring iterations: 4
```

Let’s run a bootstrap.

``` r
library(glmmboot)
set.seed(15278086) # Happy for Nadia and Alan
boot_results <- bootstrap_model(base_model = base_run, 
                                base_data = sample_frame,
                                resamples = 999)
# Performing case resampling (no random effects)
```

And the results:

``` r
print(boot_results)
#               estimate boot 2.5% boot 97.5% boot p_value base p_value
# (Intercept)  0.3403002   -0.5647     1.3247        0.516       0.5266
# x1          -0.2683814   -0.8386     0.2789        0.372       0.3566
# x2           0.2708438   -1.3674     2.0126        0.778       0.7764
#             base 2.5% base 97.5% boot/base width
# (Intercept)   -0.7327     1.4133       0.8804217
# x1            -0.8483     0.3116       0.9634510
# x2            -1.6363     2.1780       0.8861446
```

The estimates are the same, since we just pull from the base model. The
intervals are similar to the base model, although slightly narrower:
typical logistic regression is fractionally conservative at `N = 50`.

An example with a zero-inflated model (from the `glmmTMB` docs):

``` r
library(glmmTMB)

owls <- transform(Owls,
                  nest = reorder(Nest, NegPerChick),
                  ncalls = SiblingNegotiation,
                  ft = FoodTreatment)

fit_zipoisson <- glmmTMB(
    ncalls ~ (ft + ArrivalTime) * SexParent +
        offset(log(BroodSize)) + (1 | nest),
    data = owls,
    ziformula = ~1,
    family = poisson)

summary(fit_zipoisson)
#  Family: poisson  ( log )
# Formula:          
# ncalls ~ (ft + ArrivalTime) * SexParent + offset(log(BroodSize)) +  
#     (1 | nest)
# Zero inflation:          ~1
# Data: owls
# 
#      AIC      BIC   logLik deviance df.resid 
#   4015.6   4050.8  -1999.8   3999.6      591 
# 
# Random effects:
# 
# Conditional model:
#  Groups Name        Variance Std.Dev.
#  nest   (Intercept) 0.1294   0.3597  
# Number of obs: 599, groups:  nest, 27
# 
# Conditional model:
#                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.53995    0.35656   7.123 1.05e-12 ***
# ftSatiated                -0.29111    0.05961  -4.884 1.04e-06 ***
# ArrivalTime               -0.06808    0.01427  -4.771 1.84e-06 ***
# SexParentMale              0.44885    0.45002   0.997    0.319    
# ftSatiated:SexParentMale   0.10473    0.07286   1.437    0.151    
# ArrivalTime:SexParentMale -0.02140    0.01835  -1.166    0.244    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Zero-inflation model:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.05753    0.09412  -11.24   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Let’s run the bootstrap (ignore the actual results, 3 resamples is
basically meaningless - just for illustration):

``` r
zi_results <- bootstrap_model(base_model = fit_zipoisson,
                              base_data = owls,
                              resamples = 3,
                              parallelism = "future")
# Performing block resampling, over nest
print(zi_results)
# $cond
#                              estimate boot 2.5% boot 97.5% boot p_value
# (Intercept)                2.53994692    1.2486     2.9952          0.5
# ftSatiated                -0.29110639   -0.3479    -0.0040          0.5
# ArrivalTime               -0.06807809   -0.0927    -0.0192          0.5
# SexParentMale              0.44884508   -0.7715     0.9317          1.0
# ftSatiated:SexParentMale   0.10472505    0.0444     0.2690          0.5
# ArrivalTime:SexParentMale -0.02139750   -0.0430     0.0299          1.0
#                           base p_value base 2.5% base 97.5%
# (Intercept)                     0.0000    1.8411     3.2388
# ftSatiated                      0.0000   -0.4079    -0.1743
# ArrivalTime                     0.0000   -0.0960    -0.0401
# SexParentMale                   0.3186   -0.4332     1.3309
# ftSatiated:SexParentMale        0.1506   -0.0381     0.2475
# ArrivalTime:SexParentMale       0.2436   -0.0574     0.0146
#                           boot/base width
# (Intercept)                     1.2495857
# ftSatiated                      1.4714964
# ArrivalTime                     1.3134953
# SexParentMale                   0.9654847
# ftSatiated:SexParentMale        0.7864098
# ArrivalTime:SexParentMale       1.0127490
# 
# $zi
#              estimate boot 2.5% boot 97.5% boot p_value base p_value
# (Intercept) -1.057534   -1.0575     -0.788          0.5            0
#             base 2.5% base 97.5% boot/base width
# (Intercept)    -1.242    -0.8731       0.7306774
```

We could also have run this with the `future.apply` backend:

``` r
library(future.apply)
plan("multiprocess")

zi_results <- bootstrap_model(base_model = fit_zipoisson,
                              base_data = owls,
                              resamples = 1000,
                              parallelism = "future")
```
