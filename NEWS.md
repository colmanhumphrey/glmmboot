# glmmboot 0.4.0

API changes:

* `BootGlmm` is now `bootstrap_model` as the primary exported function
* `BootCI` is now `bootstrap_ci`
* Both of these functions should now work smoothly if your model returns a list of matrix coefficients; it will perform the bootstrap sampling on all of them
* You basically have to supply `base_data` to `bootstrap_model`; to reflect this, it's now the second argument, prior to `resamples`
* parallel functionality is improve, now supporting the `future.apply::future_lapply` backend (and some slightly more robust versions of e.g. `parallel::mclapply` if desired)
* Added code coverage

# glmmboot 0.3.0

* For general narrowness bias, now resampling k-1 levels of random effects / n-1 rows of regular data (depending on existence of random effects). This can be turned off with narrowness_avoid = FALSE in BootGlmm.

# glmmboot 0.2.0

* Added a warning if data not supplied explicitly
* Changed two-sided p-value calculation to more tightly respect the fact that the bootstrap t-values
  intentionally are not forced to be symmetric.
  
# Travis CI:
[![Travis-CI Build Status](https://travis-ci.org/ColmanHumphrey/glmmboot.svg?branch=master)](https://travis-ci.org/ColmanHumphrey/glmmboot)

# glmmboot 0.1.2

* Added a `NEWS.md` file to track changes to the package.
* Initial release of glmmboot



