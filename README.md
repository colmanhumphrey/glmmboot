
<!-- README.md is generated from README.Rmd. Please edit that file -->
glmmboot
========

The goal of glmmboot is to provide a simple method to create bootstrap confidence intervals using a wide set of models. For models with random effects, the default behaviour will be to block sample over the effect with the largest entropy (generally the one with the most levels); with no random effects, performs case resampling.

The only requirements are that the model works with the function update, to change the data; and that the coefficients are extractable using coef(summary(model)) or coef(summary(model))$cond. The data will be automatically extracted if of typical form; if not, you can supply it manually.

Installation
------------

For now, you can install glmmboot from github with:

``` r
# install.packages("devtools")
devtools::install_github("ColmanHumphrey/glmmboot")
```

Example
-------

We'll provide a quick example using glm. First we'll set up some data:

``` r
x1 <- rnorm(50)
x2 <- runif(50)

expit <- function(x){exp(x) / (1 + exp(x))}

y_mean <- expit(0.2 - 0.3 * x1 + 0.4 * x2)

y <- rbinom(50, 1, prob = y_mean)
```

Typically this model is fit with logistic regression:

``` r
base_run <- glm(y ~ x1 + x2, family = binomial(link = 'logit'))

summary(base_run)
#> 
#> Call:
#> glm(formula = y ~ x1 + x2, family = binomial(link = "logit"))
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -1.3838  -1.3079   0.9926   1.0290   1.1374  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)
#> (Intercept)  0.50854    0.57408   0.886    0.376
#> x1           0.01176    0.26991   0.044    0.965
#> x2          -0.41584    1.08987  -0.382    0.703
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 68.029  on 49  degrees of freedom
#> Residual deviance: 67.871  on 47  degrees of freedom
#> AIC: 73.871
#> 
#> Number of Fisher Scoring iterations: 4
```

Let's run a bootstrap. We'll set `num_cores = 1` because the model is small - parallel overhead is too expensive here.

``` r
library(glmmboot)
boot_results <- BootGlmm(base_model = base_run, resamples = 999, num_cores = 1)
```

And the results:

``` r
print(boot_results)
#>                estimate boot 2.5% boot 97.5%  boot p_value base p_value
#> (Intercept)  0.50854106   -0.4779     1.5686         0.386       0.3802
#> x1           0.01176399   -0.4940     0.5205         0.974       0.9654
#> x2          -0.41583663   -2.5389     1.6149         0.708       0.7045
#>             base 2.5% base 97.5% boot/base width
#> (Intercept)   -0.6464     1.6634       0.8860093
#> x1            -0.5312     0.5548       0.9341203
#> x2            -2.6084     1.7767       0.9472604
```

The estimates are the same, since we just pull from the base model. The intervals are similar to the base model, although slightly narrower: typical logistic regression is fractionally conservative at `N = 50`.
