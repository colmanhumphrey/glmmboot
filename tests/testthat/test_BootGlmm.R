context('Testing BootGlmm (and DetectCores, and CalcEntropy)')

test_that('BootGlmm returns matrix when asked, lists when asked', {
    x <- rnorm(10)
    y <- rnorm(10)    
    simple_model <- lm(y ~ x)
    
    expect_equal(
        class(BootGlmm(base_model = simple_model, resamples = 20)),
        'matrix')
    expect_equal(
        class(BootGlmm(base_model = simple_model, resamples = 20,
                       return_coefs_instead = TRUE)),
        'list')
})

test_that('BootGlmm fails when model has no data and none is supplied.', {
    x <- rnorm(10)
    y <- rnorm(10)    
    simple_model <- lm(y ~ x)

    simple_model_nodata <- simple_model
    simple_model_nodata$model <- NULL
    
    expect_error(
        BootGlmm(base_model = simple_model_nodata, resamples = 20)
    )
})


    
