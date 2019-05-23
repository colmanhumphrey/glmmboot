
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glmmboot

The goal of glmmboot is to provide a simple method to create bootstrap
confidence intervals using a wide set of models. For models with random
effects, the default behaviour will be to block sample over the effect
with the largest entropy (generally the one with the most levels); with
no random effects, performs case resampling. glmmboot should work for
models that produce multiple sets of coefficients too.

The only requirements are that the model works with the function
`update`, to change the data; and that the coefficients are extractable
using coef(summary(model)): either directly, or stored in it as a list.
This includes e.g. zero-inflated models, which produce two matrices of
coefficients.

It may be desired to run this package in parallel. The best way is to
use the `future` backend. You do that by specifying the backend
`future::plan` setup, and then setting `parallelism = "future"`
(although it actually calls \`future.apply::future\_lapply, so that
should be installed too for this).

While in some cases the data will be automatically extracted, you should
supply it manually.

## Installation

glmmboot is on CRAN, so you can install it normally:

``` r
install.packages("glmmboot")
```

Or the development version:

``` r
devtools::install_github("ColmanHumphrey/glmmboot")
```

## Example

We’ll provide a quick example using glm. First we’ll set up some data:

``` r
x1 <- rnorm(50)
x2 <- runif(50)

expit <- function(x){exp(x) / (1 + exp(x))}

y_mean <- expit(0.2 - 0.3 * x1 + 0.4 * x2)

y <- rbinom(50, 1, prob = y_mean)

sample_frame = data.frame(x1 = x1, x2 = x2, y = y)
```

Typically this model is fit with logistic
regression:

``` r
base_run <- glm(y ~ x1 + x2, family = binomial(link = 'logit'), data = sample_frame)

summary(base_run)
#> 
#> Call:
#> glm(formula = y ~ x1 + x2, family = binomial(link = "logit"), 
#>     data = sample_frame)
#> 
#> Deviance Residuals: 
#>      Min        1Q    Median        3Q       Max  
#> -1.26572  -1.17266  -0.01963   1.16751   1.29371  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)
#> (Intercept) -0.05062    0.58173  -0.087    0.931
#> x1          -0.15539    0.31457  -0.494    0.621
#> x2           0.02808    1.02269   0.027    0.978
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 69.315  on 49  degrees of freedom
#> Residual deviance: 69.049  on 47  degrees of freedom
#> AIC: 75.049
#> 
#> Number of Fisher Scoring iterations: 3
```

Let’s run a bootstrap.

``` r
library(glmmboot)
set.seed(15278086) # Happy for Nadia and Alan
boot_results <- bootstrap_model(base_model = base_run, 
                                base_data = sample_frame,
                                resamples = 999)
#> Performing case resampling (no random effects)
```

And the results:

``` r
print(boot_results)
#>                estimate boot 2.5% boot 97.5% boot p_value base p_value
#> (Intercept) -0.05061697   -1.1132     1.0499        0.954       0.9310
#> x1          -0.15538896   -0.7424     0.4295        0.692       0.6236
#> x2           0.02807799   -1.8579     1.9877        1.000       0.9782
#>             base 2.5% base 97.5% boot/base width
#> (Intercept)   -1.2209     1.1197       0.9241990
#> x1            -0.7882     0.4774       0.9259148
#> x2            -2.0293     2.0855       0.9345831
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
#>  Family: poisson  ( log )
#> Formula:          
#> ncalls ~ (ft + ArrivalTime) * SexParent + offset(log(BroodSize)) +  
#>     (1 | nest)
#> Zero inflation:          ~1
#> Data: owls
#> 
#>      AIC      BIC   logLik deviance df.resid 
#>   4015.6   4050.8  -1999.8   3999.6      591 
#> 
#> Random effects:
#> 
#> Conditional model:
#>  Groups Name        Variance Std.Dev.
#>  nest   (Intercept) 0.1294   0.3597  
#> Number of obs: 599, groups:  nest, 27
#> 
#> Conditional model:
#>                           Estimate Std. Error z value         Pr(>|z|)    
#> (Intercept)                2.53995    0.35656   7.123 0.00000000000105 ***
#> ftSatiated                -0.29111    0.05961  -4.884 0.00000104200662 ***
#> ArrivalTime               -0.06808    0.01427  -4.771 0.00000183764044 ***
#> SexParentMale              0.44885    0.45002   0.997            0.319    
#> ftSatiated:SexParentMale   0.10473    0.07286   1.437            0.151    
#> ArrivalTime:SexParentMale -0.02140    0.01835  -1.166            0.244    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Zero-inflation model:
#>             Estimate Std. Error z value            Pr(>|z|)    
#> (Intercept) -1.05753    0.09412  -11.24 <0.0000000000000002 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Let’s run the bootstrap (ignore the actual results, 3 resamples is
basically meaningless - just for illustration):

``` r
zi_results <- bootstrap_model(base_model = fit_zipoisson,
                              base_data = owls,
                              resamples = 3,
                              parallelism = "future")
#> Performing block resampling, over nest
print(zi_results)
#> $cond
#>                              estimate boot 2.5% boot 97.5% boot p_value
#> (Intercept)                2.53994692    2.4870     2.9761          0.5
#> ftSatiated                -0.29110639   -0.6201    -0.2911          0.5
#> ArrivalTime               -0.06807809   -0.0863    -0.0663          0.5
#> SexParentMale              0.44884508   -1.0627     0.4488          1.0
#> ftSatiated:SexParentMale   0.10472505   -0.0347     0.1728          1.0
#> ArrivalTime:SexParentMale -0.02139750   -0.0214     0.0451          1.0
#>                           base p_value base 2.5% base 97.5%
#> (Intercept)                     0.0000    1.8411     3.2388
#> ftSatiated                      0.0000   -0.4079    -0.1743
#> ArrivalTime                     0.0000   -0.0960    -0.0401
#> SexParentMale                   0.3186   -0.4332     1.3309
#> ftSatiated:SexParentMale        0.1506   -0.0381     0.2475
#> ArrivalTime:SexParentMale       0.2436   -0.0574     0.0146
#>                           boot/base width
#> (Intercept)                     0.3498814
#> ftSatiated                      1.4080066
#> ArrivalTime                     0.3581549
#> SexParentMale                   0.8568378
#> ftSatiated:SexParentMale        0.7262668
#> ArrivalTime:SexParentMale       0.9250336
#> 
#> $zi
#>              estimate boot 2.5% boot 97.5% boot p_value base p_value
#> (Intercept) -1.057534   -1.0738    -0.9611          0.5            0
#>             base 2.5% base 97.5% boot/base width
#> (Intercept)    -1.242    -0.8731       0.3055893
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
