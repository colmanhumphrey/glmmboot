context("test-bootstrap_methods")


test_that("testing gen_sample", {
    level_list <- list(num = 1:10,
                       let = letters[10:14])
    expect_equal(length(gen_sample(level_list)),
                 2)
    expect_equal(lengths(gen_sample(level_list)),
                 lengths(level_list) - 1)
    expect_equal(lengths(gen_sample(level_list, reduce_by_one = FALSE)),
                 lengths(level_list))
    expect_equal(names(gen_sample(level_list)),
                 names(level_list))

    expect_true(all(gen_sample(level_list,
                               rand_columns = "num")$num %in% level_list$num))

    expect_error(gen_sample(level_list,
                            unique_resample_lim = c(2, 3)))
    expect_error(gen_sample(level_list,
                            unique_resample_lim = c("num" = 2, "let" = 3)),
                 NA)
})


test_that("testing gen_samp_lev", {
    expect_equal(length(gen_samp_lev(letters[5:9])),
                 4)
    expect_equal(length(gen_samp_lev(letters[5:9], reduce_by_one = FALSE)),
                 5)

    expect_error(gen_samp_lev(letters[5:7], unique_lim = 3))

    ## technically a miniscule chance of failing:
    expect_error(gen_samp_lev(letters[5:7],
                              unique_lim = 3,
                              reduce_by_one = FALSE),
                 NA)
    expect_error(gen_samp_lev(letters[5:7],
                              unique_lim = 2,
                              reduce_by_one = TRUE),
                 NA)
})


test_that("testing gen_vector_match", {
    orig_vector <- c(1001, 1001, 2001, 3001, 3001, 3001)
    new_vector <- c(1001, 2001, 1001)

    vector_match <- gen_vector_match(orig_vector, new_vector)
    expect_equal(vector_match, c(1, 2, 3, 1, 2))
})


test_that("testing gen_resampling_index", {

    expect_error(gen_resampling_index(
        list(a = 1:3, b = 4:6, d = letters),
        list(a = 1:3, d = letters)))

    ##------------------------------------
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

    ##------------------------------------

    ## realistic case:
    num_total_samples <- 500000
    orig_list <- list(sample(100, num_total_samples, TRUE),
                      sample(200, num_total_samples, TRUE))
    sampled_list <- list(sample(100, 100, TRUE),
                         sample(200, 200, TRUE))
    matched_ind <- gen_resampling_index(orig_list, sampled_list)

    expect_true((abs(num_total_samples - length(matched_ind)) /
                 num_total_samples) < 0.03)

    expected_unique <- num_total_samples * (1 - exp(-1))^2
    expect_true((abs(expected_unique -
                    length(unique(matched_ind))) / expected_unique) <
                0.2)
})

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


test_that("testing find_bars", {
    expect_equal(find_bars(as.formula("y ~ x + (1 | rand_effect)")),
                 list("1 | rand_effect"))
    ## turns string to formula too
    expect_equal(find_bars("y ~ x + (1 | rand_effect)"),
                 list("1 | rand_effect"))
})


test_that("testing calc_entropy", {
    vec_5_6 <- c(5 / 6, 1 / 6)
    expect_equal(calc_entropy(c(rep("a", 5), "b")),
                 -sum(vec_5_6 * log(vec_5_6)))
})


test_that("testing list_of_matrices", {
    listlist_of_maybe_matrices <- list(
        list(matrix(1:10, 2, 5), matrix(1, 1, 1)),
        list(matrix(NA, 0, 0), matrix("also not NA", 2, 5)),
        list(cbind(1:3, rep(NA, 3))),
        list(1:20),
        list(list(matrix(10, 1, 2))))

    expect_equal(
        unlist(lapply(listlist_of_maybe_matrices, list_of_matrices)),
        c(TRUE, TRUE, TRUE, FALSE, FALSE))

    expect_false(list_of_matrices(1:10))
    expect_false(list_of_matrices(list()))
})


test_that("testing not_error_check", {
    listlist_of_matrices <- list(
        list(matrix(1:10, 2, 5), matrix(1, 1, 1)),
        list(matrix(NA, 0, 0), matrix("also not NA", 2, 5)),
        list(cbind(1:3, rep(NA, 3))))
    expect_equal(not_error_check(listlist_of_matrices),
                 c(TRUE, TRUE, FALSE))
})
