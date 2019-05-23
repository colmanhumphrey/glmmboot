context("test-bootstrap_ci")

test_that("testing bootstrap_ci", {
    x <- rnorm(10)
    y <- rnorm(10)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    list_out  <- bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE)

    expect_equal(class(bootstrap_ci(list_out$base_coef_se,
                                    list_out$resampled_coef_se)),
                 "matrix")
})

test_that("testing combine_resampled_lists", {
    x <- rnorm(10)
    y <- rnorm(10)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    list_out1 <- bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE)
    list_out2 <- bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE)
    list_out3 <- bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE)

    expect_equal(combine_resampled_lists(list_out1, list_out2, list_out3),
                 combine_resampled_lists(list(list_out1, list_out2, list_out3)))
})
