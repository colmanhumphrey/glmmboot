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
#'   summary(model_output)$coefficients$cond[,1:2, drop = FALSE]
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
#' @return A matrix containing:
#'   Estimates;
#'   Bootstrap interval endpoints;
#'   Bootstrap p-value;
#'   Base p-value;
#'   Base interval endpoints;
#'   Relative width of bootstrap interval to base
#'
#' @examples
#'   BootCI(
#' 
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

#' takes in a list of unique levels in the random columns,
#' gives back a random sampling of each
GenSamples <- function(list_of_levels,
                       resamples,
                       rand_columns = NULL,
                       unique_resample_lim = NULL,
                       num_cores = 4){
    if(is.null(rand_columns)){
        rand_columns = names(list_of_levels)
    }

    gen_samp_lev <- function(levs, unique_lim){
        if(!is.null(unique_lim)){
            len_uni = unique_lim - 1
            iters = 0
            while(len_uni < unique_lim &
                  iters < 100){
                      if(iters == 99){
                          stop('lower unique requirement')
                      } else {
                          iters = iters + 1
                      }
                      samp = sample(levs, replace = TRUE)
                      len_uni = length(unique(samp))
                  }
        } else {
            samp = sample(levs, replace = TRUE)
        }
        return(samp)
    }

    gen_samp <- function(rand_cols, lev_list, unique_resample_lim){
        temp = lapply(rand_cols, function(rc){gen_samp_lev(lev_list[[rc]], unique_resample_lim[rc])})
        names(temp) = rand_cols
        return(temp)
    }
    
    if('parallel' %in% (.packages())){
        rc_list =  mclapply(1:resamples,
                            function(i){gen_samp(rand_columns,
                                                 list_of_levels,
                                                 unique_resample_lim)},
                            mc.cores = num_cores)
    } else {
        rc_list = lapply(1:resamples,
                         function(i){gen_samp(rand_columns,
                                                 list_of_levels,
                                                 unique_resample_lim)})
    }

    return(rc_list)
}

#' this function takes in original vectors, and resampled editions,
#' it spits back the matching index of the original variables for the new resampled ones
GenResamplingIndex <- function(orig_list,
                               sampled_list){
    if(length(orig_list) != length(sampled_list)){
        stop('lists must be the same length (the original variables and the sampled variables)')
    }
    
    orig_strings <- do.call(paste, orig_list)
    sampled_strings <- do.call(paste, expand.grid(sampled_list))

    ## don't want to call parallel version, since this function
    ## is called within parallel func.
    ## if('parallel' %in% (.packages())){
    ##     all_ind_list <- mclapply(sampled_strings, function(str){
    ##         which(orig_strings == str)}, mc.cores = num_cores)
    ## } else {
    ##     all_ind_list <- lapply(sampled_strings, function(str){
    ##         which(orig_strings == str)})
    ## }
    all_ind_list <- lapply(sampled_strings, function(str){
        which(orig_strings == str)})

    all_ind = unlist(all_ind_list)

    return(all_ind)
}

#' this takes in the result of the function findbars() on a formula,
#' and gives back the plain names of the columns
GetRand <- function(findbar_list){
    return(unlist(
        lapply(findbar_list,
               function(x){as.character(x[3])})
    ))
}
