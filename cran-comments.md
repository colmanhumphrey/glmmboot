## Release Summary

For the most part, functionality is not fundamentally changed in terms of the results from the primary functions in common situations.

Some functionality changes:
* gen_resampling_index is now better in many ways, including more performative, but should return the same results as before
* parallel backends more robustly setup, including support for `future.apply::future_lapply`
* The code now works correctly in situations where the base model returns a list of coefficients

Naming changes:
* Any function that didn't have a snake_case name now has one, including most notably the two main exported functions, `bootstrap_model` and `bootstrap_ci`. The old version still exist and are marked as deprecated.
* very mild reordering of the arguments for bootstrap_model, to reflect that the base_data should be supplied in basically all circumstances

Further: added code coverage

## Test environments
* local OS X install, R 3.5.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None so far.

---
