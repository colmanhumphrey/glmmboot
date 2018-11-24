# glmmboot 0.3.0

* For general narrowness bias, now resampling k-1 levels of random effects / n-1 rows of regular data (depending on existence of random effects). This can be turned off with narrowness_avoid = FALSE in BootGlmm.

# glmmboot 0.2.0

* Added a warning if data not supplied explictly
* Changed two-sided p-value calculation to more tightly respect the fact that the bootstrap t-values
  intentionally are not forced to be symmetric.
  
# Travis CI:
[![Travis-CI Build Status](https://travis-ci.org/ColmanHumphrey/glmmboot.svg?branch=master)](https://travis-ci.org/ColmanHumphrey/glmmboot)

# glmmboot 0.1.2

* Added a `NEWS.md` file to track changes to the package.
* Initial release of glmmboot



