## Update
In this version:
* Added a warning if data not supplied explictly (base_data argument in BootGlmm)
* Changed two-sided p-value calculation to more tightly respect the fact that the bootstrap t-values
  intentionally are not forced to be symmetric (BootCI)


## Test environments
* local OS X install, R 3.5.0
* Ubuntu 14.04.5 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None so far.

---
