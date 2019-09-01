#' Generates block resamples
#'
#' Takes in a list of unique levels in the random columns,
#' gives back a random sampling of each.
#'
#' @param list_of_levels
#'   list of vectors of levels of each random effect
#'
#' @param rand_columns
#'   Default NULL.
#'   name of columns to randomise over; if NULL, will use
#'   all in 'list_of_levels'
#'
#' @param unique_resample_lim
#'   Default NULL; optionally set a minimal number of unique levels each sample
#'   must produce. Note that it should be a named vector,
#'   the same as the levels to randomise over.
#'
#' @param reduce_by_one
#'   Logical, default TRUE; for (potentially) more accurate coverage,
#'   resample one less than the number of levels in each list
#'
#' @return
#'   A list of samples
#'
#' @keywords internal
gen_sample <- function(list_of_levels,
                       rand_columns = NULL,
                       unique_resample_lim = NULL,
                       reduce_by_one = TRUE){
    if (is.null(rand_columns)) {
        rand_columns <- names(list_of_levels)
    }

    temp <- lapply(rand_columns,
                   function(rc){
                       gen_samp_lev(list_of_levels[[rc]],
                                    unique_resample_lim[rc],
                                    reduce_by_one)
                   })
    names(temp) <- rand_columns

    temp
}

#' For resampling from a single set of levels
#'
#' @param levels
#'   The levels to sample
#'
#' @param unique_lim
#'   Default NULL; optionally a required number of unique
#'   elements to have in the sample
#'
#' @inheritParams gen_sample
#'
#' @return
#' A sample of 'levels'; a vector.
#'
#' @keywords internal
gen_samp_lev <- function(levels,
                         unique_lim = NULL,
                         reduce_by_one = TRUE){
    if (!is.null(unique_lim)) {
        len_uni <- unique_lim - 1
        iters <- 0
        while (len_uni < unique_lim & iters < 100) {
            if (iters == 99) {
                stop("lower unique requirement")
            } else {
                iters <- iters + 1
            }
            if (reduce_by_one) {
                samp <- sample(levels,
                               size = length(levels) - 1,
                               replace = TRUE)
            } else {
                samp <- sample(levels, replace = TRUE)
            }
            len_uni <- length(unique(samp))
        }
    } else {
        if (reduce_by_one) {
            samp <- sample(levels, size = length(levels) - 1, replace = TRUE)
        } else {
            samp <- sample(levels, replace = TRUE)
        }
    }
    return(samp)
}



#' Finds all occurrences of new_vector in orig_vector
#'
#' For each value in new_vector, we find the indices of ALL
#' matching values in orig_vector. This means that if new_vector
#' has duplicates, we'll duplicate the indices from orig_vector too
#'
#' @param orig_vector vector to find indices from
#' @param new_vector vector to match values to (from orig_vector)
#' @param current_index accumulator of the indices so far, for recursion
#' @return returns a vector of indices from orig_vector that correspond to
#' values in new_vector
#'
#' @examples
#'
#' \donttest{
#'     orig_vector <- c(1, 1, 2, 3, 3, 3)
#'     new_vector <- c(1, 2, 1)
#'
#'     vector_match <- gen_vector_match(orig_vector, new_vector)
#'     ## testthat::expect_equal(vector_match, c(1, 2, 3, 1, 2))
#' }
#'
#' @keywords internal
gen_vector_match <- function(orig_vector,
                             new_vector,
                             current_index = vector("integer", 0)){
    if (length(new_vector) == 0){
        return(current_index)
    }
    ## this is TCOable, but the trampolining version was twice as slow
    dups <- duplicated(new_vector)
    gen_vector_match(orig_vector,
                     new_vector[dups],
                     c(current_index,
                       which(orig_vector %in% new_vector[!dups])))
}


#' Given resampled vectors, gives matching index of original variables
#'
#' this function takes in original vectors, and resampled editions,
#' it spits back the matching index of the original variables
#' for the new resampled ones
#'
#' @param orig_list
#' List of original data vectors
#'
#' @param sampled_list
#' Sampled list
#'
#' @return
#' Returns an index vector
#'
#' @keywords internal
gen_resampling_index <- function(orig_list,
                                 sampled_list){
    if (length(orig_list) != length(sampled_list)) {
        stop("lists must be the same length ",
             "(the original variables and the sampled variables)",
             call. = FALSE)
    }

    ## coercing to character means this works for all types
    ## but we have to worry about accidental equality
    ## e.g. ("a b", "c") vs ("a" "b c") (and even worse, all "a")
    paste_under <- function(...){
        paste(..., sep = "_")
    }
    orig_strings <- paste0(do.call(paste, orig_list),
                           do.call(paste_under, rev(orig_list)))
    sampled_strings <- paste0(
        do.call(paste, expand.grid(sampled_list)),
        do.call(paste_under, rev(expand.grid(sampled_list))))

    gen_vector_match(orig_strings, sampled_strings)
}


#' this takes in a formula with bars
#' and gives back the plain names of the columns
#'
#' @param form_with_bars
#' A formula used in e.g. lme4 and similar
#' packages. Typically along the lines:
#' y ~ age + (1 | school)
#' etc
#'
#' @return A vector of the variables that
#' are treated as random
#'
#' @examples
#' get_rand("y ~ age + (1 | school)")
#' get_rand("y ~ income + (1 | school) + (1 | school:section)")
#' get_rand("y ~ income + (1 | school) + (1 | school/section)")
#' get_rand(as.formula("y ~ x + (1 | z)"))
#' get_rand("y ~ x")
#'
#' @export
get_rand <- function(form_with_bars){
    findbar_list <- find_bars(form_with_bars)

    first_pass <- unlist(
        lapply(findbar_list,
               function(x){
                   if (class(x) == "call") {
                       return(as.character(x[3])) # nocov
                   } else {
                       first_bar <- unlist(gregexpr("|", x, fixed = TRUE))[[1]]
                       return(trimws(
                           substr(x, start = first_bar + 1, stop = nchar(x))))
                   }
               }))
    ## potentially we'll have : and so on
    all_vars <- all.vars(as.formula(form_with_bars))

    in_firstpass <- unlist(lapply(all_vars, function(y){
        any_match <- unlist(lapply(first_pass, function(x){
            grepl(y, x, fixed = TRUE)}))
        return(sum(any_match) > 0)}))

    all_vars[in_firstpass]
}

#' Returns the terms with bars from a formula
#'
#' @inheritParams get_rand
#'
#' @keywords internal
find_bars <- function(form_with_bars){
    ## if it's just text right now, convert it
    if (!("formula" %in% class(form_with_bars))) {
        form_with_bars <- as.formula(form_with_bars)
    }

    ## get the terms
    form_terms <- attributes(terms(form_with_bars))$term.labels

    ## return the terms with a bar
    as.list(form_terms[grepl("|", form_terms, fixed = TRUE)])
}

#' Calculate Shannon Entropy
#' @keywords internal
calc_entropy <- function(level_vector){
    freq_as_prob <- table(level_vector) / length(level_vector)

    -sum(freq_as_prob * log(freq_as_prob))
}

#' Checks that an object is a list of matrices
#'
#' Checks that an object is a list, and also
#' that the list is a collection of matrices.
#' Currently returns FALSE on an empty list
#'
#' @param list_to_check
#' The "list" (maybe!) to check
#'
#' @param allow_null
#' If an element is NULL, is that OK?
#'
#' @return
#' TRUE or FALSE
#'
#' @keywords internal
list_of_matrices <- function(list_to_check,
                             allow_null = TRUE){
    if (!("list" %in% class(list_to_check))) {
        return(FALSE)
    }

    if (length(list_to_check) == 0) {
        return(FALSE)
    }

    all(unlist(lapply(list_to_check, function(maybe_mat){
        if ("matrix" %in% class(maybe_mat)) {
            return(TRUE)
        }
        if (allow_null && is.null(maybe_mat)) {
            return(TRUE)
        }
        FALSE
    })))
}

#' Checks if the result of bootstrap_coef_est is not error
#'
#' For each element of the list of results
#' from running bootstrap_coef_est, checks if it's
#' a list of matrices, and that each matrix has no missing values
#'
#' @param coef_list_list
#' list of results from running bootstrap_coef_est,
#' e.g. lapply(1:N, bootstrap_coef_est)
#'
#' @return
#' A logical vector, TRUE if the element is indeed a list of matrices
#' with non-missing entries
#'
#' @keywords internal
not_error_check <- function(coef_list_list){
    unlist(lapply(coef_list_list, function(coef_list){
        all(unlist(lapply(coef_list, function(coef_maybe_mat){
            "matrix" %in% class(coef_maybe_mat) &&
                !anyNA(coef_maybe_mat)
        })))
    }))
}
