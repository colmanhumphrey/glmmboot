#' Generating bootstrap confidence intervals.
#'
#' Enter first level estimates and second level estimates,
#' get bootstrap interval, from the pivotal bootstrap t
#' (Efron and Tibshirani 1994, also endorsed
#' by Hesterberg 2015).
#' @param base_coef_se Estimates and SEs from full sample. In matrix form,
#'   i.e. a \eqn{(p+1) x 2} matrix, first column is estimates,
#'   second is standard errors. This is the output from using:
#'   \code{coef(summary(model_output))[,1:2, drop = FALSE]}
#'   or
#'   \code{coef(summary(model_output))$cond[,1:2, drop = FALSE]}
#'   if \code{model_output} is the output from a random
#'   effects model (some may not have \code{cond} as the correct pull).
#' @param resampled_coef_se List of estimates and SEs from the bootstrapped
#'   resamples, each list entry has the same format as the base_coef_se above.
#' @param orig_df Degrees of freedom to use to calculate the
#'   t-values used for the base interval.
#' @param alpha_level
#'   level of CI - if you fill in \code{probs}, will use those instead
#' @param probs Default \code{NULL}, and will use \code{alpha_level} to set
#'   endpoints. Else will calculate these CI endpoints.
#' @return A matrix containing:
#'   \itemize{
#'    \item Estimates
#'    \item Bootstrap interval endpoints
#'    \item Bootstrap p-value
#'    \item Base p-value
#'    \item Base interval endpoints
#'    \item Relative width of bootstrap interval to base
#'   }
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20) + x
#' xy_data = data.frame(x = x, y = y)
#' first_model <- lm(y ~ x, data = xy_data)
#' out_list <- bootstrap_model(first_model, base_data = xy_data, 20,
#'                            return_coefs_instead = TRUE)
#' bootstrap_ci(out_list$base_coef_se,
#'              out_list$resampled_coef_se)
#'
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   ## where subj is a random effect
#'   test_model <- glmmTMB(y ~ x_var1 + (1 | subj),
#'                         data = test_data, family = binomial)
#'   output_lists <- bootstrap_model(test_model, base_data = test_data, 199,
#'                                   return_coefs_instead = TRUE)
#'   bootstrap_ci(output_lists$base_coef_se,
#'                output_lists$resampled_coef_se)
#' }
#'
#' @export
bootstrap_ci <- function(base_coef_se = NULL,
                         resampled_coef_se = NULL,
                         orig_df = NULL,
                         alpha_level = 0.05,
                         probs = NULL){
    resampled_coef_lists <- lapply(
        1:length(resampled_coef_se[[1]]), function(j){
            lapply(resampled_coef_se, `[[`, j)
        })

    ci_results <- Map(function(base_matrix, resampled_coef_list){
        bootstrap_individual_ci(base_matrix,
                                resampled_coef_list,
                                orig_df = orig_df,
                                alpha_level = alpha_level,
                                probs = probs)
    }, base_coef_se, resampled_coef_lists)

    ## controversial: in many cases there's just one list, so just send it
    if (length(ci_results) == 1) {
        return(ci_results[[1]])
    }

    ci_results
}

#' Runs the bootstrap estimation method for a single set of coefs (not a list)
#'
#' @inheritParams bootstrap_ci
#' @return Returns a matrix result
#' @keywords internal
bootstrap_individual_ci <- function(base_matrix = NULL,
                                    resampled_coef_list = NULL,
                                    orig_df = NULL,
                                    alpha_level = 0.05,
                                    probs = NULL){
    if (is.null(probs)) {
        if (alpha_level < 0 | alpha_level > 0.5) {
            stop("Can't calculate a two-sided CI with this alpha value, ",
                 "must be in (0, 0.5)", call. = FALSE)
        }
        probs <- sort(c(alpha_level / 2, 1 - alpha_level / 2))
    }
    if (max(probs) > 1 || min(probs) < 0) {
        stop("Probabilities should be in (0,1)", call. = FALSE)
    }

    base_row_names <- rownames(base_matrix)
    all_row_names <- lapply(resampled_coef_list, rownames)

    name_match <- unlist(lapply(all_row_names, function(x){
        mean(x == base_row_names) == 1
    }))

    if (mean(name_match) != 1) {
        stop("Naming mismatch from base to list of coefs", call. = FALSE)
    }

    resampled_ests_vecs <- lapply(1:nrow(base_matrix), function(j){
        unlist(lapply(resampled_coef_list, function(x){
            x[j, 1]
        }))
    })
    resampled_ses_vecs <- lapply(1:nrow(base_matrix), function(j){
        unlist(lapply(resampled_coef_list, function(x){
            x[j, 2]
        }))
    })

    conf_list <- Map(function(base_est,
                              base_se,
                              resampled_ests,
                              resampled_ses){
        ci_variable(base_est,
                    base_se,
                    resampled_ests,
                    resampled_ses,
                    probs = probs)
    },
    base_matrix[, 1],
    base_matrix[, 2],
    resampled_ests_vecs,
    resampled_ses_vecs)

    conf_ind <- do.call(rbind, conf_list)
    rownames(conf_ind) <- rownames(base_matrix) ## only needed for nrow == 1...
    colnames(conf_ind) <- c(
        paste0("boot ",
               names(quantile(1, probs))),
        "boot p_value"
    )

    use_df <- ifelse(is.null(orig_df), Inf, orig_df)
    t_base_vals <- qt(probs, df = use_df)

    if (nrow(base_matrix) > 1) {
        base_mat <- cbind(
            sapply(t_base_vals, function(x){
                base_matrix[, 1] + x * base_matrix[, 2]
            }),
            2 * pt(-abs(base_matrix[, 1] / base_matrix[, 2]),
                   df = use_df)
        )
    } else {
        base_mat <- matrix(c(
            sapply(t_base_vals, function(x){
                base_matrix[, 1] + x * base_matrix[, 2]
            }),
            2 * pt(-abs(base_matrix[, 1] / base_matrix[, 2]),
                   df = use_df)
        ), nrow = 1)
    }

    colnames(base_mat) <- c(
        paste0("base ",
               names(quantile(1, probs))),
        "base p_value"
    )

    if (nrow(base_matrix) > 1) {
        ret_matrix <- cbind(
            "estimate" = base_matrix[, 1],
            round(conf_ind, 4),
            round(base_mat[, c(3, 1, 2)], 4),
            "boot/base width" =
                (conf_ind[, 2] - conf_ind[, 1]) /
                (base_mat[, 2] - base_mat[, 1])
        )
    } else {
        ret_matrix <- matrix(c(
            base_matrix[, 1],
            round(conf_ind, 4),
            round(base_mat[, c(3, 1, 2)], 4),
            (conf_ind[, 2] - conf_ind[, 1]) /
            (base_mat[, 2] - base_mat[, 1])
        ), nrow = 1)
        colnames(ret_matrix) <- c(
            "estimate",
            "boot 2.5%", "boot 97.5%",
            "boot p_value", "base p_value",
            "base 2.5%", "base 97.5%",
            "boot/base width")
        rownames(ret_matrix) <- rownames(base_matrix)
    }

    ret_matrix
}

#' Gets the confidence interval and p-value for a single variable.
#' @param base_est Base model estimate.
#' @param base_se Base model SE.
#' @param resampled_ests Vector of estimates from resampling.
#' @param resampled_ses Vector of standard errors from resampling.
#' @return Returns a length 3 vector: left and right CI value, and p-value.
#' @keywords internal
ci_variable <- function(base_est,
                        base_se,
                        resampled_ests,
                        resampled_ses,
                        probs){
    ## thanks Tim!
    t_boot <- quantile((resampled_ests - base_est) / resampled_ses,
                       1 - probs,
                       type = 6)
    ## can't have the CI not containing the estimate...!
    ## shouldn't happen too often...
    t_boot[1] <- ifelse(t_boot[1] < 0, 0, t_boot[1])
    t_boot[2] <- ifelse(t_boot[2] > 0, 0, t_boot[2])

    result <- base_est - t_boot * base_se

    ## p val:
    p_t <- base_est / base_se

    p_val_num_left <- sum((resampled_ests - base_est) /
                          resampled_ses <=  p_t) + 1
    p_val_num_right <- sum((resampled_ests - base_est) /
                           resampled_ses >=  p_t) + 1

    p_val_num <- 2 * min(p_val_num_left, p_val_num_right)

    p_val <- p_val_num / (length(resampled_ests) + 1)

    c(result, p_val)
}


#' @rdname bootstrap_ci
#' @export
#' @param alp_level
#' now alpha_level
BootCI <- function(base_coef_se = NULL, # nocov start
                   resampled_coef_se = NULL,
                   orig_df = NULL,
                   alp_level = 0.05,
                   probs = NULL){
    .Deprecated("bootstrap_ci")
    bootstrap_ci(base_coef_se = base_coef_se,
                 resampled_coef_se = resampled_coef_se,
                 orig_df = orig_df,
                 alpha_level = alp_level,
                 probs = probs)
} # nocov end


#' Combines output from multiple bootstrap_model calls
#'
#' If you run glmmboot on e.g. a grid of computers,
#' set \code{return_coefs_instead = TRUE} for each.
#' Then enter them all here. Either just list them out,
#' or put them into one list and enter them.
#' @param ... List of outputs to be combined, or just a bunch of output entries
#'   as separate unnamed arguments.
#' @param return_combined_list Logical, default \code{FALSE}.
#'   \code{TRUE} if you want the combined list of lists,
#'   \code{FALSE} for just the output from bootstrap_ci applied to it.
#' @return Returns the same output as \code{bootstrap_ci} by default,
#'   or the combined list (as if you had just run bootstrap_model once with
#'   all resamples) if \code{return_combined_list = TRUE}
#' @examples
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   ## where subj is some RE
#'   test_model <- glmmTMB(y ~ x_var1 + (1 | subj),
#'                         data = test_data,
#'                         family = binomial)
#'   output_list1 <- bootstrap_model(
#'       test_model, base_data = test_data, 99, return_coefs_instead = TRUE)
#'   output_list2 <- bootstrap_model(
#'       test_model, base_data = test_data, 100, return_coefs_instead = TRUE)
#'   output_list3 <- bootstrap_model(
#'       test_model, base_data = test_data, 100, return_coefs_instead = TRUE)
#'   combine_resampled_lists(output_list1, output_list2, output_list3)
#'
#'   num_blocks = 10
#'   num_total_resamples = 299
#'   reg_list <- list()
#'   for(i in 1:num_blocks){
#'       if(i < num_blocks){
#'           block_resamples = floor((num_total_resamples + 1)/num_blocks)
#'       } else {
#'           block_resamples = floor((num_total_resamples + 1)/num_blocks - 1)
#'       }
#'       reg_list[[i]] = bootstrap_model(test_model,
#'                                       base_data = test_data,
#'                                       resamples = block_resamples,
#'                                       return_coefs_instead = TRUE,
#'                                       num_cores = 1) ## increase for parallel
#'   }
#'   boot_ci1 <- combine_resampled_lists(reg_list)
#'   full_list <- combine_resampled_lists(reg_list, return_combined_list = TRUE)
#'   boot_ci2 <- bootstrap_ci(full_list$base_coef_se,
#'                            full_list$resampled_coef_se)
#'   identical(boot_ci1, boot_ci2)
#' }
#'
#' @export
combine_resampled_lists <- function(...,
                                    return_combined_list = FALSE){
    input_lists <- list(...)

    if (length(input_lists) == 1) {
        input_lists <- input_lists[[1]]
    }

    reg_base_coef <- input_lists[[1]]$base_coef_se

    ## now a list of lists of the resampled:
    listlist_resampled <- lapply(input_lists, "[[", 2)
    ## combine to one list
    reg_resampled <- do.call(c, listlist_resampled)

    if (return_combined_list) {
        return(list(base_coef_se = reg_base_coef,
                    resampled_coef_se = reg_resampled))
    }

    bootstrap_ci(base_coef_se = reg_base_coef,
                 resampled_coef_se = reg_resampled)
}

#' @rdname combine_resampled_lists
#' @export
CombineResampledLists <- function(..., # nocov start
                                  return_combined_list = FALSE){
    .Deprecated("combine_resampled_lists")
    combine_resampled_lists(...,
                            return_combined_list = return_combined_list)
} # nocov end
