context("test-bootstrap_model")

test_that("bootstrap_model returns matrix when asked, lists when asked", {
    x <- rnorm(10)
    y <- rnorm(10)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    expect_equal(
        class(bootstrap_model(base_model = simple_model,
                              base_data = xy_data,
                              resamples = 20)),
        "matrix")
    expect_equal(
        class(bootstrap_model(base_model = simple_model,
                              base_data = xy_data,
                              resamples = 20,
                              return_coefs_instead = TRUE)),
        "list")
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

    library(glmmTMB)

    ## just copying the model from the glmmTMB vignettes etc
    owls <- transform(Owls,
                      nest = reorder(Nest, NegPerChick),
                      ncalls = SiblingNegotiation,
                      ft = FoodTreatment)

    fit_zipoisson <- glmmTMB(
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

    expect_equal(unname(unlist(lapply(zero_boot, class))),
                 c("matrix", "matrix"))
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
})
