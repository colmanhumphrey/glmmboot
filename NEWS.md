# glmmboot 0.6.0

* Removing previously deprecated functions (deprecated as of 0.4.0)
* Adding conditional evaluation for tests and the vignette (and the readme)
  for `glmmTMB`, in case it's not present / installable on a given system (as
  it's only a suggested package)
* Removing `call. = FALSE` for errors and warnings
* adding `future.seed = TRUE` to `future.apply::future_lapply` call for better RNG
* Minor vignette and README copy edits

# glmmboot 0.5.1

* Changing a `donttest` to a `dontrun` within an example.

# glmmboot 0.5.0

* if `parallelism = "parallel"` but `num_cores` is left as NULL, we default to using `parallel::detectCores() - 1L` cores. This was in fact the documented behaviour previously, just sadly not the actual behaviour
* `bootstrap_model` (and internal function `bootstrap_runner`) accepts an argument `future_packages` that will be passed along to `future.apply::future.lapply`; this is needed for futures that don't share memory, because the required global isn't visible (when using S3 generics...). This is also passed along to README/vignettes/tests etc.
* Some documentation edits, shouldn't affect functionality
* sampling message shown by default in interactive settings
* fixing all incorrect uses of `class` (new matrix class is the biggest change) - mostly was in tests!

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
