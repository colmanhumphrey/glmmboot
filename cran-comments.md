## Update
In this version:
* Added a warning if data not supplied explictly (base_data argument in BootGlmm)
* Changed two-sided p-value calculation to more tightly respect the fact that the bootstrap t-values
  intentionally are not forced to be symmetric (BootCI)


## Test environments
* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None so far.

## Travis CI:
[![Travis-CI Build Status](https://travis-ci.org/ColmanHumphrey/glmmboot.svg?branch=master)](https://travis-ci.org/ColmanHumphrey/glmmboot)

---
