context("test-bootstrap_methods")

test_that("testing get_rand", {

    expect_equal(get_rand("y ~ age + (1 | school)"),
                 "school")

    expect_equal(get_rand("y ~ income + (1 | school) + (1 | school:section)"),
                 c("school", "section"))

    expect_equal(get_rand("y ~ income + (1 | school) + (1 | school/section)"),
                 c("school", "section"))

    expect_equal(get_rand(as.formula("y ~ x + (1 | z)")),
                 "z")

    ## i.e. empty vector
    expect_equal(get_rand("y ~ x"),
                 vector(mode = "character"))
})

test_that("testing gen_resampling_index", {
    factor_1 <- factor(LETTERS[rep(5:14, each = 10)])
    factor_2 <- factor(
        c("apple", "banana", "carrot", "durian")[rep(1:4, times = 25)])

    sample_list <- list(factor_1[c(1, 11, 21, 1)],
                        factor_2[1])

    indexing <- gen_resampling_index(orig_list = list(factor_1, factor_2),
                                     sampled_list = sample_list)

    efg_ind <- lapply(c(1, 11, 21), function(j){
        which(factor_1 == factor_1[j])
    })
    apple_ind <- which(factor_2 == factor_2[1])

    ## E shows up twice
    res_ind <- c(rep(intersect(efg_ind[[1]], apple_ind), times = 2),
                 intersect(efg_ind[[2]], apple_ind),
                 intersect(efg_ind[[3]], apple_ind))
    expect_equal(sort(indexing), sort(res_ind))

    ##------------------------------------

    factor_1 <- factor(LETTERS[rep(5:14, each = 10)])
    factor_2 <- factor(
        c("apple", "banana", "carrot", "durian")[rep(1:4, each = 25)])

    sample_list <- list(factor_1[c(1, 11, 21, 31)],
                        factor_2[c(51, 76)])

    indexing <- gen_resampling_index(orig_list = list(factor_1, factor_2),
                                     sampled_list = sample_list)

    ## the sampled pair never shows up
    expect_equal(indexing, integer(0))
})
