context("test-bootstrap_ci")


test_that("testing bootstrap_ci", {
    x <- rnorm(20)
    y <- rnorm(20)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    list_out  <- bootstrap_model(base_model = simple_model,
                                 base_data = xy_data,
                                 resamples = 20,
                                 return_coefs_instead = TRUE)

    expect_true(inherits(bootstrap_ci(list_out$base_coef_se,
                                      list_out$resampled_coef_se),
                         "matrix"))
})


test_that("testing combine_resampled_lists", {
    x <- rnorm(20)
    y <- rnorm(20)
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

    expect_equal(combine_resampled_lists(list_out1, list_out2, list_out3,
                                         return_combined_list = TRUE),
                 combine_resampled_lists(list(list_out1, list_out2, list_out3),
                                         return_combined_list = TRUE))
})


test_that("testing interior components", {
    x <- rnorm(20)
    y <- rnorm(20)
    xy_data <- data.frame(x = x, y = y)
    simple_model <- lm(y ~ x, data = xy_data)

    some_results <- bootstrap_model(base_model = simple_model,
                                    base_data = xy_data,
                                    resamples = 20,
                                    return_coefs_instead = TRUE)
    just_cond_list <- lapply(some_results$resampled_coef_se, `[[`, "cond")
    base_mat <- some_results$base_coef_se$cond

    expect_error(bootstrap_individual_ci(
        base_matrix = some_results$base_coef_se$cond,
        resampled_coef_list = just_cond_list),
        NA)
    expect_error(bootstrap_individual_ci(base_matrix = base_mat,
                                         resampled_coef_list = just_cond_list,
                                         alpha_level = 1))
    expect_error(bootstrap_individual_ci(base_matrix = base_mat,
                                         resampled_coef_list = just_cond_list,
                                         alpha_level = -1))
    expect_error(bootstrap_individual_ci(base_matrix = base_mat,
                                         resampled_coef_list = just_cond_list,
                                         probs = c(-2, 2)))
    rownames(base_mat) <- paste0("bad_", rownames(base_mat))
    expect_error(bootstrap_individual_ci(base_matrix = base_mat,
                                         resampled_coef_list = just_cond_list))
})
