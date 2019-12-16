#' Computes bootstrap resamples of your data,
#' stores estimates + SEs.
#'
#' By default, this will compute bootstrap resamples
#' and then send them to \code{bootstrap_ci}
#' for calculation.
#' @import methods
#' @import stats
#' @param base_model The pre-bootstrap model, i.e. the model output
#'   from running a standard model call.
#'   Examples:
#'   \code{base_model <- glmmTMB(y ~ age + (1 | subj),
#'                         data = rel_data, family = binomial)}
#'   \code{base_model <- lm(y ~ x, data = xy_frame)}
#' @param base_data The data that was used in the call. You
#'   can leave this to be automatically read, but
#'   I highly recommend supplying it
#' @param resamples How many resamples of your data do you want to do?
#'   9,999 is a reasonable default (see Hesterberg 2015),
#'   but start very small to make sure it works on
#'   your data properly, and to get a rough timing estimate etc.
#' @param return_coefs_instead Logical, default \code{FALSE}: do you want the
#'   list of lists of results for each bootstrap sample (set to \code{TRUE}),
#'   or the matrix output of all samples? See return for more details.
#' @param parallelism Type of parallelism (if any) to use to run the resamples.
#'   Options are:
#'   \describe{
#'     \item{\code{"none"}}{The default, sequential}
#'     \item{\code{"future"}}{To use \code{future.apply} (\code{future}s)}
#'     \item{\code{"parallel"}}{To use \code{parallel::mclapply}}
#'   }
#' @param resample_specific_blocks Character vector, default \code{NULL}.
#'   If left \code{NULL}, this algorithm with choose ONE random block to resample over -
#'   the one with the largest entropy (often the one with most levels).
#'   If you wish to resample over specific random effects as blocks, enter
#'   the names here - can be one, or many. Note that resampling
#'   multiple blocks is in general quite conservative.
#'   If you want to perform case resampling but you DO have
#'   random effects, set \code{resample_specific_blocks} to any
#'   non-null value that isn't equal to a random effect
#'   variable name.
#' @param unique_resample_lim Should be same length as number of random effects
#'   (or left \code{NULL}).
#'   Do you want to force the resampling to produce a minimum number of
#'   unique values in sampling? Don't make this too big.
#'   Must be named same as rand cols
#' @param narrowness_avoid Boolean, default \code{TRUE}. If \code{TRUE}, will resample n-1
#'   instead of n elements in the bootstrap (n being either rows,
#'   or random effect levels,
#'   depending on existence of random effects). If \code{FALSE}, will do
#'   typical size n resampling.
#' @param num_cores How many cores to use.
#'   Defaults to \code{parallel::detectCores() - 1L} if
#'   \code{parallelism = "parallel"}
#' @param future_packages Packages to pass to created futures when
#'   using \code{parallelism = "future"}. This must be supplied if
#'   the package used to model the data isn't in base and you're
#'   using a plan that doesn't have shared memory, because the
#'   model is updated with the S3 generic \code{update}.
#' @param suppress_sampling_message Logical, the default is
#'   to suppress if not in an interactive session.
#'   Do you want the function to message the console with the type of
#'   bootstrapping? If block resampling over random effects, then it'll say
#'   what effect it's sampling over; if case resampling -
#'   in which case it'll say as much.
#'   Set \code{TRUE} to hide message.
#' @return By default (with \code{return_coefs_instead} being \code{FALSE}),
#'   returns the output from \code{bootstrap_ci};
#'   for each set of covariates (usually just the one set,
#'   the conditional model) we get a matrix of output: a row for each variable
#'   (including the intercept),
#'   estimate, CIs for boot and base, p-values.
#'   If \code{return_coefs_instead} is \code{TRUE}, then will instead
#'   return a list of length two:
#'   \itemize{
#'     \item a list containing the output for the base model
#'     \item a list of length \code{resamples} each a list of matrices of
#'       estimates and standard errors for each model.
#'   }
#'   This output is useful for error checking, and if you want
#'   to run this function in certain distributed ways.
#'
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20) + x
#' xy_data = data.frame(x = x, y = y)
#' first_model <- lm(y ~ x, data = xy_data)
#'
#' out_matrix <- bootstrap_model(first_model, base_data = xy_data, 20)
#' out_list <- bootstrap_model(first_model,
#'                             base_data = xy_data,
#'                             resamples = 20,
#'                             return_coefs_instead = TRUE)
#'
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   test_formula <- as.formula('y ~ x_var1 + x_var2 + x_var3 + (1|subj)')
#'   test_model <- glmmTMB(test_formula, data = test_data, family = binomial)
#'   output_matrix <- bootstrap_model(test_model, base_data = test_data, 199)
#'
#'   output_lists <- bootstrap_model(test_model,
#'                                   base_data = test_data,
#'                                   resamples = 199,
#'                                   return_coefs_instead = TRUE)
#' }
#'
#' @export
bootstrap_model <- function(base_model,
                            base_data,
                            resamples = 9999,
                            return_coefs_instead = FALSE,
                            parallelism = c("none", "future", "parallel"),
                            resample_specific_blocks = NULL,
                            unique_resample_lim = NULL,
                            narrowness_avoid = TRUE,
                            num_cores = NULL,
                            future_packages = NULL,
                            suppress_sampling_message = !interactive()){
    if (missing(base_data) || is.null(base_data)) {
        if ("model" %in% names(base_model)) {
            base_data <- base_model$model
        } else if ("frame" %in% names(base_model)) {
            base_data <- base_model$frame # nocov
        } else if ("frame" %in% slotNames(base_model)) {
            base_data <- base_model@frame # nocov
        } else {
            stop("base_data cannot be automatically inferred, ",
                 "please supply data as base_data ",
                 "to this function", call. = FALSE)
        }

        warning("Please supply data through the argument base_data; ",
                "automatic reading from your model can produce ",
                "unforeseeable bugs.", call. = FALSE)
    }

    if (missing(parallelism)) {
        if (!is.null(num_cores) && num_cores > 1) {
            ## if just the num_cores argument is used, we'll
            ## assume parallel::mclapply is desired
            if (!requireNamespace("parallel", quietly = TRUE)) { # nocov start
                stop("setting `num_cores` greater than 1 without setting ",
                     "`parallelism` uses `package:parallel`, ",
                     "but it's not installed", call. = FALSE)
            }
            parallelism <- "parallel"  # nocov end
        } else {
            parallelism <- "none"
        }
    } else {
        parallelism <- match.arg(parallelism)
        if (parallelism == "none" && !is.null(num_cores) && num_cores > 1) {
            stop("contradiction between `parallelism = \"none\"` ",
                 "and `num_cores = ", num_cores, "`; please resolve",
                 call. = FALSE)
        }
        if (parallelism == "future") {
            if (!requireNamespace("future.apply", quietly = TRUE)) { # nocov start
                stop("`parallelism = \"future\"` uses `package:future.apply`, ",
                     "but it's not installed", call. = FALSE)
            } # nocov end
            if (!is.null(num_cores)) {
                stop("with `parallelism = \"future\"`, the `num_cores` ",
                     "argument is not used to set up the backend; ",
                     "use `future::plan` instead",
                     call. = FALSE)
            }
        }
        if (parallelism == "parallel") {
            if (!requireNamespace("parallel", quietly = TRUE)) { # nocov start
                stop("`parallelism = \"parallel\"` uses `package:parallel`, ",
                     "but it's not installed", call. = FALSE)
            } # nocov end

            if (is.null(num_cores)) { # nocov start
                num_cores <- max(parallel::detectCores() - 1L, 1L)
                message("`num_cores` not set, defaulting to ", num_cores,
                        " (`parallel::detectCores() - 1L`)")
            } # nocov end
        }
    }

    if (parallelism != "future" && !is.null(future_packages)) {
        stop("Argument `future_packages` should only be set when ",
             "using `parallelism = \"future\"`", call. = FALSE)
    }

    ##------------------------------------

    ## formula processing
    boot_form <- formula(base_model)
    rand_cols <- get_rand(boot_form)

    ## base regression
    base_coef <- coef(summary(base_model))

    ## this is where we have to decide how to get the coefs.
    if (is.matrix(base_coef)) {
        extract_coef <- function(model){
            list(cond = coef(summary(model))[, 1:2, drop = FALSE])
        }

        main_coef_se <- extract_coef(base_model)
    } else {
        if (!list_of_matrices(base_coef)) {
            stop("currently this method needs `coef(summary(base_model))` ", # nocov start
                 "to be a matrix, or a list of them", call. = FALSE) # nocov end
        }
        ## only calc not_null once, but local scope the result
        extract_coef <- (function(not_null){
            function(model){
                lapply(coef(summary(model))[not_null], function(coef_mat){
                    coef_mat[, 1:2, drop = FALSE]
                })
            }
        })(not_null = !unlist(lapply(base_coef, is.null)))

        main_coef_se <- extract_coef(base_model)
    }

    ##------------------------------------

    ## deciding on random blocks. Subset of rand_cols:
    if (is.null(resample_specific_blocks)) {
        if (length(rand_cols) > 1) {
            entropy_levels <- unlist(lapply(rand_cols, function(rc){
                calc_entropy(base_data[, rc])
            }))
            ## takes the first in a tie, for consistency.
            rand_cols <- rand_cols[which.max(entropy_levels)]
        }
    } else {
        if (sum(rand_cols %in% resample_specific_blocks) == 0 &&
            length(rand_cols) > 0) {
            stop("No random columns from formula found ",
                 "in resample_specific_blocks", call. = FALSE)
        }
        rand_cols <- rand_cols[rand_cols %in% resample_specific_blocks]
    }

    ## if rand_cols is not empty, we'll resample the blocks
    ## if it's empty, we'll do standard case resampling
    if (length(rand_cols) > 0) {
        if (!suppress_sampling_message) {
            message("Performing block resampling, over ",
                    paste(rand_cols, collapse = ", "))
        }

        orig_list <- lapply(rand_cols, function(rand_col){
            base_data[, rand_col]
        })
        all_list <- lapply(orig_list, unique)
        names(orig_list) <- rand_cols
        names(all_list) <- rand_cols

        gen_sample_data <- (function(base_data,
                                     orig_list,
                                     all_list,
                                     rand_cols,
                                     unique_resample_lim,
                                     narrowness_avoid){
            function(){
                sample_list <- gen_sample(all_list,
                                          rand_cols,
                                          unique_resample_lim,
                                          narrowness_avoid)
                base_data[
                    gen_resampling_index(orig_list, sample_list), ]
            }
        })(base_data,
            orig_list,
            all_list,
            rand_cols,
            unique_resample_lim,
            narrowness_avoid)
    } else {
        if (!suppress_sampling_message) {
            message("Performing case resampling (no random effects)")
        }

        gen_sample_data <- (function(base_data,
                                     narrowness_avoid){
            if (narrowness_avoid) {
                return(function(){
                    base_data[sample(nrow(base_data),
                                     nrow(base_data) - 1,
                                     replace = TRUE), ]
                })
            }
            function(){
                base_data[sample(nrow(base_data),
                                 replace = TRUE), ]
            }
        })(base_data, narrowness_avoid)
    }

    bootstrap_coef_est <- (function(base_model, gen_sample_data){
        function(){
            sample_data <- gen_sample_data()
            model_output <- suppressWarnings(update(base_model,
                                                    data = sample_data))
            extract_coef(model_output)
        }
    })(base_model, gen_sample_data)

    ##------------------------------------

    coef_se_list <- bootstrap_runner(bootstrap_coef_est,
                                     resamples = resamples,
                                     parallelism = parallelism,
                                     num_cores = num_cores,
                                     future_packages = future_packages)

    ##------------------------------------

    ## some could be errors
    error_ind <- !not_error_check(coef_se_list)

    if (mean(error_ind) > 0.25) {
        warning("There area lot of errors (approx ", # nocov start
                round(100 * mean(error_ind), 1), "%)") # nocov end
    }

    ## keep going until solved
    max_redos <- 15L
    redo_iter <- 1L
    while (sum(error_ind) > 0L && redo_iter <= max_redos) {
        message(sum(error_ind), " error(s) to redo")
        redo_iter <- redo_iter + 1L

        coef_se_list[error_ind] <- bootstrap_runner(bootstrap_coef_est,
                                                    sum(error_ind),
                                                    parallelism,
                                                    num_cores)

        error_ind <- !not_error_check(coef_se_list)
    }
    if (any(error_ind)) {
        stop("could not generate error-free resamples in ", # nocov start
             max_redos, " attempts",
             call. = FALSE) # nocov end
    }

    if (return_coefs_instead) {
        return(list(base_coef_se = main_coef_se,
                    resampled_coef_se = coef_se_list))
    }

    ## We won't dive too far into the world of dfs,
    ## we'll basically default to z-values (Inf df) if not clear
    if ("df.residual" %in% names(base_model)) {
        orig_df <- base_model$df.residual
    } else {
        orig_df <- Inf
    }

    bootstrap_ci(base_coef_se = main_coef_se,
                 resampled_coef_se = coef_se_list,
                 orig_df = orig_df)
}


#' @export
#' @rdname bootstrap_model
#' @param suppress_loading_bar
#' defunct now
#' @param allow_conv_error
#' defunct now
BootGlmm <- function(base_model, # nocov start
                     resamples = 9999,
                     base_data = NULL,
                     return_coefs_instead = FALSE,
                     resample_specific_blocks = NULL,
                     unique_resample_lim = NULL,
                     narrowness_avoid = TRUE,
                     num_cores = NULL,
                     suppress_sampling_message = FALSE,
                     suppress_loading_bar = FALSE,
                     allow_conv_error = FALSE){
    .Deprecated("bootstrap_model")

    bootstrap_model(base_model = base_model,
                    base_data = base_data,
                    resamples = resamples,
                    return_coefs_instead = return_coefs_instead,
                    resample_specific_blocks = resample_specific_blocks,
                    unique_resample_lim = unique_resample_lim,
                    narrowness_avoid = narrowness_avoid,
                    num_cores = num_cores,
                    suppress_sampling_message = suppress_sampling_message)
} # nocov end


#' Runs the bootstrapping of the models.
#'
#' This function gets passed a function that runs a single bootstrap resample
#' and a number of resamples, and decides how to run them
#' e.g. in parallel etc.
#' @param bootstrap_function Function that we wish to run
#'   \code{resamples} times.
#' @inheritParams bootstrap_model
#' @return Returns the list that contains the results of running
#'   \code{bootstrap_function}.
#' @keywords internal
bootstrap_runner <- function(bootstrap_function,
                             resamples,
                             parallelism = c("none", "future", "parallel"),
                             num_cores = NULL,
                             future_packages = NULL){
    parallelism <- match.arg(parallelism)

    if (parallelism == "future") {
        ## assuming end-user sets plan elsewhere
        return(future.apply::future_lapply(
                                 1:resamples,
                                 function(i){
                                     bootstrap_function()
                                 },
                                 future.packages = future_packages))
    }

    if (parallelism == "parallel") {
        return(parallel::mclapply(1:resamples,
                                  function(i){
                                      bootstrap_function()
                                  },
                                  mc.cores = num_cores,
                                  mc.preschedule = FALSE))
    }

    lapply(1:resamples, function(i) bootstrap_function())
}
