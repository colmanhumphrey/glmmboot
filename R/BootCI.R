#' Enter first level estimates and second level estimates,
#' get bootstrap interval, from the pivotal bootstrap t
#' (Efron and Tibshirani 1994, also endorsed
#' by Hesterberg 2015).
#'
#' @param base_coef_se
#'   Estimates and SEs from full sample. In matrix form:
#'   i.e. a (p+1) x 2 matrix, first column is estimates,
#'   second is standard errors. This
#'   is the output from using:
#'   coef(summary(model_output))[,1:2, drop = FALSE]
#'   or
#'   coef(summary(model_output))$cond[,1:2, drop = FALSE]
#'   if model_output is the output from a random
#'   effects model (some may not have cond as the correct pull).
#'
#' @param resampled_coef_se
#'   List of estimates and SEs from the bootstrapped resamples,
#'   each list entry has the same format as the base_coef_se above.
#'
#' @param orig_df
#'   Degrees of freedom to use to calculate the
#'   t-values used for the base interval.
#'
#' @param alp_level
#'   level of CI - if you fill in \code{probs}, will use those instead
#'
#' @param probs
#'   Default NULL, and will use alp_level to set
#'   endpoints. Else will calculate these CI endpoints.
#'
#' @export
#'
#' @return A matrix containing:
#'   Estimates;
#'   Bootstrap interval endpoints;
#'   Bootstrap p-value;
#'   Base p-value;
#'   Base interval endpoints;
#'   Relative width of bootstrap interval to base
#'
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20) + x
#' first_model <- lm(y ~ x)
#' out_list <- BootGlmm(first_model, 20, return_coefs_instead = TRUE)
#' BootCI(out_list$base_coef_se,
#'        out_list$resampled_coef_se)
#'
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   test_model <- glmmTMB(y ~ x + (1 | some_RE), data = test_data, family = binomial)
#'   output_lists <- BootGlmm(test_model, 199, return_coefs_instead = TRUE)
#'   BootCI(output_lists$base_coef_se,
#'          output_lists$resampled_coef_se)
#' }
BootCI <- function(base_coef_se = NULL,
                   resampled_coef_se = NULL,
                   orig_df = NULL,
                   alp_level = 0.05,
                   probs = NULL){
    if(is.null(probs)){
        if(alp_level < 0 | alp_level > 0.5){
            stop("Can't calculate a two-sided CI with this alpha value, must be in (0, 0.5)")
        }
        probs = sort(c(alp_level / 2, 1 - alp_level/2))
        ## probs = c(alp_level / 2, 1 - alp_level/2)
    }
    if(max(probs) > 1 | min(probs) < 0){
        stop("Probabilities should be in (0,1)")
    }

    base_row_names = rownames(base_coef_se)
    all_row_names <- lapply(resampled_coef_se, rownames)

    name_match <- unlist(lapply(all_row_names, function(x){mean(x == base_row_names) == 1}))

    if(mean(name_match) != 1){
        stop("Naming mismatch from base to list of coefs")
    }

    conf_ind <- matrix(NA, nrow = length(base_row_names),
                       ncol = length(probs) + 1)
    rownames(conf_ind) = base_row_names

    ## actual work here...:
    for(var in base_row_names){
        base_est = base_coef_se[var,1]
        base_se = base_coef_se[var,2]

        rep_ests = unlist(lapply(resampled_coef_se, function(x){x[var,1]}))
        rep_ses = unlist(lapply(resampled_coef_se, function(x){x[var,2]}))

        ## thanks Tim!
        t_boot <- quantile( (rep_ests - base_est) / rep_ses, 1 - probs, type = 6)
        ## can't have the CI not containing the estimate...!
        ## shouldn't happen too often...
        t_boot[1] = ifelse(t_boot[1] < 0, 0, t_boot[1])
        t_boot[2] = ifelse(t_boot[2] > 0, 0, t_boot[2])

        result <- base_est - t_boot * base_se

        ## p val...
        p_t = base_est / base_se
        ## p_val = mean(abs((rep_ests - base_est) / rep_ses) >=  abs(p_t))
        p_val_num = sum(abs((rep_ests - base_est) / rep_ses) >=  abs(p_t)) + 1
        p_val = p_val_num / (length(rep_ests) + 1)

        conf_ind[var,] = c(result, p_val)
    }
    colnames(conf_ind) = c(paste0('boot ',names(quantile(1, probs))), ' boot p_value')

    use_df = ifelse(is.null(orig_df), Inf, orig_df)
    t_base_vals = qt(probs, df = use_df)
    base_mat <- cbind(sapply(t_base_vals, function(x){base_coef_se[,1] + x * base_coef_se[,2]}),
                      2 * pt(-abs(base_coef_se[,1] / base_coef_se[,2]),
                                              df = use_df))
    colnames(base_mat) = c(paste0('base ',names(quantile(1, probs))), 'base p_value')

    ## p_star <- GenStar(conf_ind[,3])

    ret_matrix <- cbind('estimate' = base_coef_se[,1], round(conf_ind, 4),
                        ## p_star,
                        round(base_mat[,c(3,1,2)], 4),
                        'boot/base width' = (conf_ind[,2] - conf_ind[,1]) / (base_mat[,2] - base_mat[,1]))

    return(ret_matrix)
}

#' When running BootGlmm in distributed fashion,
#' combines output
#'
#' If you run BootGlmm on e.g. a grid of computers,
#' set return_coefs_instead = TRUE for each.
#' Then enter them all here. Either just list them out,
#' or put them into one list and enter them.
#'
#' @param ...
#'   Say our output from BootGlmm from three separate computers is
#'   output_list1,
#'   output_list2,
#'   output_list3
#'   We can run: CombineResampledLists(output_list1, output_list2, output_list3)
#'   OR: create a list of lists:
#'   output_list_list <- list(output_list1, output_list2, output_list3)
#'   and then: CombineResampledLists(output_list_list)
#'
#' @param return_combined_list
#'   Logical, default FALSE. TRUE if you want the combined
#'   list of lists, FALSE for just the output from BootCI applied to it.
#'
#' @export
#'
#' @return Returns the same output as BootCI by default,
#'   or the combined list (as if you had just run BootGlmm once with
#'   all resamples) if return_combined_list = TRUE
#'
#' @examples
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   test_model <- glmmTMB(y ~ x + (1 | some_RE), data = test_data, family = binomial)
#'   output_list1 <- BootGlmm(test_model, 99, return_coefs_instead = TRUE)
#'   output_list2 <- BootGlmm(test_model, 100, return_coefs_instead = TRUE)
#'   output_list3 <- BootGlmm(test_model, 100, return_coefs_instead = TRUE)
#'   CombineResampledLists(output_list1, output_list2, output_list3)
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
#'       reg_list[[i]] = BootGlmm(test_model,
#'                                resamples = block_resamples,
#'                                return_coefs_instead = TRUE,
#'                                num_cores = 4,
#'                                suppress_loading_bar = TRUE)
#'   }
#'   boot_ci1 <- CombineResampledLists(reg_list)
#'   full_list <- CombineResampledLists(reg_list, return_combined_list = TRUE)
#'   boot_ci2 <- BootCI(full_list$base_coef_se, full_list$resampled_coef_se)
#'   identical(boot_ci1, boot_ci2)
#' }
CombineResampledLists <- function(...,
                                  return_combined_list = FALSE){
    input_lists <- list(...)

    if(length(input_lists) == 1){
        input_lists <- input_lists[[1]]
    }

    reg_base_coef <- input_lists[[1]]$base_coef_se

    ## now a list of lists of the resampled:
    listlist_resampled = lapply(input_lists, '[[', 2)
    ## combine to one list
    reg_resampled = do.call(c, listlist_resampled)

    if(return_combined_list){
        return(list(base_coef_se = reg_base_coef,
                    resampled_coef_se = reg_resampled))
    } else {
        block_reg_results <- BootCI(base_coef_se = reg_base_coef,
                                    resampled_coef_se = reg_resampled)

        return(block_reg_results)
    }
}
