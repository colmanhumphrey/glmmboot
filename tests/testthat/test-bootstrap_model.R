context("test-bootstrap_model")


test_that("bootstrap_model returns matrix when asked, lists when asked", {
    x <- rnorm(10)
    y <- rnorm(10)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    expect_true(
        inherits(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20),
                 "matrix"))
    expect_true(
        inherits(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE),
                 "list"))

    expect_true(
        inherits(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 narrowness_avoid = FALSE,
                                 return_coefs_instead = TRUE),
                 "list"))
})


test_that("bootstrap_model works on test_data", {
    if (!requireNamespace("glmmTMB", quietly = TRUE)) {
        skip("need glmmTMB to be installed for default test_data run")
    }

    data(test_data)

    model_formula <- as.formula(y ~ x_var1 + x_var2 + x_var2 + (1 | subj))
    base_run <- suppressWarnings(glmmTMB::glmmTMB(formula = model_formula,
                                                  data = test_data,
                                                  family = binomial))

    expect_error(bootstrap_model(base_model = base_run,
                                 base_data = test_data,
                                 resamples = 2,
                                 suppress_sampling_message = FALSE),
                 NA)
    test_run <- bootstrap_model(base_model = base_run,
                                base_data = test_data,
                                resamples = 20)
    expect_is(test_run, "matrix")
    ## estimate doesn't change:
    expect_equal(test_run[, "estimate"],
                 coef(summary(base_run))$cond[, "Estimate"])

    ##------------------------------------
    ## adding second random effect, and using it
    targ_data <- test_data
    targ_data$targ <- sample(letters[1:10], size = 300, replace = TRUE)

    targ_model_formula <- as.formula(y ~ x_var1 + x_var2 + x_var2 +
                                    (1 | subj) + (1 | targ))
    base_run <- suppressWarnings(glmmTMB::glmmTMB(formula = targ_model_formula,
                                         data = targ_data,
                                         family = binomial))
    expect_error(bootstrap_model(base_model = base_run,
                                base_data = targ_data,
                                resamples = 20),
                 NA)

    test_run <- bootstrap_model(base_model = base_run,
                                base_data = targ_data,
                                resamples = 20,
                                resample_specific_blocks = "targ")
    expect_is(test_run, "matrix")
    ## estimate doesn't change:
    expect_equal(test_run[, "estimate"],
                 coef(summary(base_run))$cond[, "Estimate"])

    expect_error(bootstrap_model(base_model = base_run,
                                 base_data = targ_data,
                                 resamples = 20,
                                 resample_specific_blocks = "unknown_var"))

    ##------------------------------------
    ## hitting some errors, thus redoing etc

    small_data <- test_data[1:6, ]
    small_base_run <- suppressWarnings(glmmTMB::glmmTMB(formula = model_formula,
                                                        data = test_data,
                                                        family = binomial))
    expect_warning(bootstrap_model(base_model = small_base_run,
                                   base_data = small_data,
                                   resamples = 20))
})


test_that("bootstrap_model fails when there is no data", {
    x <- rnorm(10)
    y <- rnorm(10)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    ## warning if there is
    expect_warning(bootstrap_model(base_model = simple_model,
                                   resamples = 20))

    simple_model_nodata <- simple_model
    simple_model_nodata$model <- NULL

    expect_error(
        suppressWarnings(bootstrap_model(base_model = simple_model_nodata,
                                         resamples = 20))
    )
})


test_that("bootstrap_model works on zero-inflated models", {
    if (!requireNamespace("glmmTMB", quietly = TRUE)) {
        skip("need glmmTMB to be installed for zero-inflated")
    }

    ## just copying the model from the glmmTMB vignettes etc
    data(Owls, package = "glmmTMB")
    owls <- transform(Owls,
                      nest = reorder(Nest, NegPerChick),
                      ncalls = SiblingNegotiation,
                      ft = FoodTreatment)

    fit_zipoisson <- glmmTMB::glmmTMB(
                                  ncalls ~ (ft + ArrivalTime) * SexParent +
                                      offset(log(BroodSize)) + (1 | nest),
                                  data = owls,
                                  ziformula = ~1,
                                  family = poisson)

    zero_boot <- bootstrap_model(base_model = fit_zipoisson,
                                 base_data = owls,
                                 resamples = 20)
    expect_type(zero_boot, "list")
    expect_equal(names(zero_boot), c("cond", "zi"))

    expect_true(all(unlist(lapply(zero_boot, is.matrix))))
})


test_that("bootstrap_model parallelism modes", {
    x <- rnorm(20)
    y <- rnorm(20)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "none",
                                 num_cores = 4))
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "none",
                                 num_cores = 1,
                                 suppress_sampling_message = TRUE),
                 NA)
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "none",
                                 num_cores = NULL,
                                 suppress_sampling_message = TRUE),
                 NA)

    if (!requireNamespace("future.apply", quietly = TRUE)) {
        skip("need future.apply for this next test")
    }

    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "future",
                                 num_cores = 4,
                                 suppress_sampling_message = TRUE))
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "future",
                                 num_cores = NULL,
                                 suppress_sampling_message = TRUE),
                 NA)
    ## we're not actually using glmmTMB here but for testing it's fine
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "parallel",
                                 num_cores = NULL,
                                 future_packages = "glmmTMB",
                                 suppress_sampling_message = TRUE))
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "future",
                                 num_cores = NULL,
                                 future_packages = "glmmTMB",
                                 suppress_sampling_message = TRUE),
                 NA)

    skip_on_os("windows")
    expect_error(bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 parallelism = "parallel",
                                 num_cores = 2,
                                 suppress_sampling_message = TRUE),
                 NA)
    expect_error(bootstrap_model(
        base_model = simple_model,
        base_data = xy_data,
        resamples = 20,
        num_cores = 2L,
        parallelism = "parallel",
        suppress_sampling_message = FALSE),
        NA)
})
