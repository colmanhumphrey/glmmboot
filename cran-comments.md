## Release Summary

As requested, fixes issues where `class()` was assumed to return a length-one result (mostly was used in testing). All should be done now.

Apart from that, primary change is to add a better default for `parallelism = "parallel"`, and to provide a solve for issues with `parallelism = "future"`:

* if `parallelism = "parallel"` but `num_cores` is left as NULL, we default to using `parallel::detectCores() - 1L` cores. This was in fact the documented behaviour previously, just sadly not the actual behaviour
* `bootstrap_model` (and internal function `bootstrap_runner`) accepts an argument `future_packages` that will be passed along to `future.apply::future.lapply`; this is needed for futures that don't share memory, because the required global isn't visible (when using S3 generics). This is also passed along to README/vignettes/tests etc.

Some very minor documentation and messaging edits rounds out the submission.

## Test environments
* local OS X install, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* win-builder (devel and release), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None so far.

---
