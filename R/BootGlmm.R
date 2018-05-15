#' computes bootstrap resamples of your data,
#' stores estimates + SEs
#'
#' @param boot_data
#' data as you feed it to glmer etc
#'
#' @param boot_formula
#' formula as you would feed it to glmer, as a string
#'
#' @param boot_function
#' what function to use? try glmmTMB...
#'
#' @param boot_link
#' link function desired
#'
#' @param control_list
#' control as you would feed it to glmer
#'
#' @param allow_conv_error
#' logical, default FALSE; if the subsample runs but gives a convergence error,
#' is this acceptable? or should we resample?
#'
#' @param unique_resample_lim
#' should be same length as number of random effects (or left NULL).
#' Do you want to force the resampling to produce a minimum number of
#' unique values in sampling? Don't make this too big...
#' Must be named same as rand cols
BootGLM <- function(boot_data,
                    boot_formula,
                    boot_link = 'logit',
                    allow_conv_error = FALSE,
                    unique_resample_lim = NULL,
                    resamples = 9999,
                    num_cores = detectCores() - 1,
                    suppress_loading_bar = FALSE,
                    return_coefs_instead = FALSE,
                    ...){
    ## formula processing
    boot_form <- as.formula(boot_formula)
    rand_cols <- GetRand(findbars(boot_form)) ## findbars is lme4 function
    rand_cols = rand_cols[rand_cols %in% names(boot_data)]
    for(rc in rand_cols){
        boot_data[, rc] <- as.character(boot_data[, rc])
    }

    ## base regression
    ## you've probably already run this somewhere

    base_reg <- suppressWarnings(glmmTMB(data = boot_data,
                                         family = binomial(link = boot_link),
                                         formula = boot_form))

    main_coef_se = summary(base_reg)$coefficients$cond[,1:2, drop = FALSE]

    orig_list <- lapply(rand_cols, function(x){boot_data[,x]})
    all_list <- lapply(orig_list, unique)
    names(orig_list) = rand_cols
    names(all_list) = rand_cols

    ## you can't run this function without samp_data in the larger environment,
    ## I should try file a bug
    ## UPDATE: as of dec 2017, fixed!
    BTGLMCoefEst <- function(samp_list){
        samp_data = boot_data[GenResamplingIndex(orig_list, samp_list),]
        model_output = glmmTMB(data = samp_data,
                               formula = boot_form,                       
                               family = binomial(link = boot_link))
        ret_output = summary(model_output)$coefficients$cond[,1:2, drop = FALSE]
        return(ret_output)
    }
        
    sample_list_of_lists <- GenSamples(all_list,
                                       resamples,
                                       rand_cols,
                                       unique_resample_lim,
                                       num_cores = num_cores)
    
    ## message('Computing regressions')
    if(suppress_loading_bar){
        coef_se_list <- mclapply(sample_list_of_lists,
            BTGLMCoefEst, mc.cores = num_cores, mc.preschedule = FALSE)                
    } else {
        coef_se_list <- pblapply(sample_list_of_lists,
            BTGLMCoefEst, cl = num_cores, mc.preschedule = FALSE)
    }

    ## some could be errors
    coef_class <- unlist(lapply(coef_se_list, class))
    error_ind <- coef_class != 'matrix'

    if(mean(error_ind) > 0.25){
        warning('There area lot of errors (approx ', round(100 * mean(error_ind), 3), '%)')
    }

    ## keep going until solved
    while(sum(error_ind) > 0){
        message(sum(error_ind), ' error(s) to redo')

        redone_samples <- GenSamples(all_list, sum(error_ind), rand_cols,
                                     unique_resample_lim, num_cores)
        
        if(suppress_loading_bar){
            coef_se_list[error_ind] <- mclapply(redone_samples,
                                                BTGLMCoefEst, mc.cores = num_cores, mc.preschedule = FALSE)
        } else {
            coef_se_list[error_ind] <- pblapply(redone_samples,
                                                BTGLMCoefEst, cl = num_cores, mc.preschedule = FALSE)
        }

        coef_class <- unlist(lapply(coef_se_list, class))
        error_ind <- coef_class != 'matrix'
    }

    if(return_coefs_instead){
        return(list(base_coef_se = main_coef_se,
                    resampled_coef_se = coef_se_list))
    } else {
        res_mat <- BootCI(base_coef_se = main_coef_se,
                          resampled_coef_se = coef_se_list)
        
        return(res_mat)
    }
}

## there's a bug in the code for glmmTMB... you need to have samp_data (used in function) in env
## samp_data = data.frame(a = 2)
## fixed!! (dec 17)
