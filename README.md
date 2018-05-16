
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

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```
