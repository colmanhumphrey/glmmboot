#' computes bootstrap resamples of your data,
#' stores estimates + SEs.
#'
#' By default, this will compute bootstrap resamples and then send them to BootCI
#' for calculation. Note - only use parallel methods if your
#' model is expensive to build, otherwise the overhead won't be worth it.
#'
#' @import methods
#' @import stats
#'
#' @param base_model
#'   The pre-bootstrap model, i.e. the model output
#'   from running a standard model call.
#'   Examples:
#'   base_model <- glmmTMB(y ~ age + (1 | subj), data = rel_data, family = binomial)
#'   base_model <- lm(y ~ x)
#'   Assumes the data is accessible by
#'   base_model$model (typical lm or glm), or
#'   base_model$frame (works for glmmTMB), or
#'   base_model@frame (works for lmer, glmer)
#'   For now, if you have a different case, extract and send as base_data.
#'
#' @param resamples
#'   How many resamples of your data do you want to do?
#'   9999 is a reasonable default (see Hesterberg 2015),
#'   but start very small to make sure it works on
#'   your data properly, and to get a rough timing estimate etc.
#'
#' @param base_data
#'   Default NULL; In a future version this will become mandatory to supply.
#'   In some cases, it can be extracted
#'   from the base_model, but this can produce bugs. It's recommended that
#'   you supply your data.
#'
#' @param return_coefs_instead
#'   Logical, default FALSE: do you want the list of lists
#'   of results for each bootstrap sample (set to TRUE), or the
#'   matrix output of all samples? See return for more details.
#'
#' @param resample_specific_blocks
#'   Character vector, default NULL. If left NULL,
#'   this algorithm with choose ONE random block to resample over -
#'   the one with the largest entropy (often the one with most levels).
#'   If you wish to
#'   resample over specific random effects as blocks, enter
#'   the names here - can be one, or many. Note that resampling
#'   multiple blocks is in general quite conservative.
#'
#'   If you want to perform case resampling but you do have
#'   random effects, set resample_specific_blocks to any
#'   non-null value that does not contain any random effect
#'   variable names.
#'
#' @param unique_resample_lim
#'   Should be same length as number of random effects (or left NULL).
#'   Do you want to force the resampling to produce a minimum number of
#'   unique values in sampling? Don't make this too big...
#'   Must be named same as rand cols
#'
#' @param num_cores
#'   Defaults to detectCores() - 1 if parallel is loaded,
#'   or just 1 if not. How many cores to use if doing parallel work?
#'
#' @param suppress_sampling_message
#'   Logical, default FALSE. By default, this function
#'   will message the console with the type of bootstrapping:
#'   block resampling over random effects - in which case it'll say
#'   what effect it's sampling over;
#'   case resampling - in which case it'll say as much.
#'   Set TRUE to hide message.
#'
#' @param suppress_loading_bar
#'   Logical, default FALSE. If TRUE, uses standard
#'   mclapply for parallel work, else uses pblapply (from pbapply),
#'   which provides a nice loading bar for progress.
#'
#' @param allow_conv_error
#'   logical, default FALSE; if the subsample runs but gives a convergence error,
#'   is this acceptable? or should we resample?
#'
#' @export
#'
#' @return Returns the output from BootCI:
#'   a matrix of output for each fixed variable, including
#'   the intercept (estimate, CIs for boot and base, p-values).
#'   If return_coefs_instead = TRUE, then will instead
#'   return a list of length two:
#'   [[1]] will be a matrix containing the base
#'   estimates and standard errors.
#'   [[2]] will be a list of length resamples,
#'   each a matrix of estimates and standard errors.
#'   This output is useful for error checking, and if you want
#'   to run this function in a distributed way.
#'
#' @examples
#' x <- rnorm(20)
#' y <- rnorm(20) + x
#' xy_data = data.frame(x = x, y = y)
#' first_model <- lm(y ~ x, data = xy_data)
#'
#' out_matrix <- BootGlmm(first_model, 20, base_data = xy_data)
#' out_list <- BootGlmm(first_model, 20, base_data = xy_data, return_coefs_instead = TRUE)
#'
#' \donttest{
#'   data(test_data)
#'   library(glmmTMB)
#'   test_formula <- as.formula('y ~ x_var1 + x_var2 + x_var3 + (1|subj)')
#'   test_model <- glmmTMB(test_formula, data = test_data, family = binomial)
#'   output_matrix <- BootGlmm(test_model, 199, base_data = test_data)
#'
#'   output_lists <- BootGlmm(test_model, 199, base_data = test_data, return_coefs_instead = TRUE)
#' }
BootGlmm <- function(base_model,
                     resamples = 9999,
                     base_data = NULL,
                     return_coefs_instead = FALSE,
                     resample_specific_blocks = NULL,
                     unique_resample_lim = NULL,
                     num_cores = DetectCores() - 1,
                     suppress_sampling_message = FALSE,
                     suppress_loading_bar = FALSE,
                     allow_conv_error = FALSE){
    ## formula processing
    boot_form <- formula(base_model)
    rand_cols <- GetRand(boot_form)

    ## base regression
    rel_coef <- coef(summary(base_model))
    ## ideally this is already the fixed effects matrix, else we pull 'cond'
    if(is.matrix(rel_coef)){
        main_coef_se <- rel_coef[,1:2, drop = FALSE]
    } else {
        if('cond' %in% names(rel_coef)){
            main_coef_se = rel_coef$cond[,1:2, drop = FALSE]
        } else {
            stop('Other coefficient extraction methods not implemented, please file an issue')
        }
    }

    if(is.null(base_data)){
        if('model' %in% names(base_model)){
            base_data <- base_model$model
        } else if('frame' %in% names(base_model)){
            base_data <- base_model$frame
        } else if('frame' %in% slotNames(base_model)){
            base_data <- base_model@frame
        } else {
            stop('Other dataframe extraction methods not implemented, please file an issue, or supply data as base_data to this function')
        }
        warning('Please supply data through the argument base_data; automatic reading from your model can produce unforeseeable bugs. In a future version, explicit data passing will become mandatory.')
    }

    ## deciding on random blocks. Subset of rand_cols:
    if(is.null(resample_specific_blocks)){
        entropy_levels <- unlist(lapply(rand_cols, function(rc){CalcEntropy(base_data[,rc])}))
        rand_cols <- rand_cols[which.max(entropy_levels)] ## takes the first in a tie, for consistency.
    } else {
        if(sum(rand_cols %in% resample_specific_blocks) == 0 && length(rand_cols) > 0){
            stop('No random columns from formula found in resample_specific_blocks')
        }
        rand_cols <- rand_cols[rand_cols %in% resample_specific_blocks]
    }

    ## if rand_cols is not empty, we'll resample the blocks
    ## if it's empty, we'll do standard case resampling
    if(length(rand_cols) > 0){
        message('Performing block resampling, over ', paste(rand_cols, collapse = ', '))
        orig_list <- lapply(rand_cols, function(x){base_data[,x]})
        all_list <- lapply(orig_list, unique)
        names(orig_list) = rand_cols
        names(all_list) = rand_cols

        BTCoefEst <- function(){
            samp_list = GenSample(all_list,
                                   rand_cols,
                                   unique_resample_lim)

            samp_data = base_data[GenResamplingIndex(orig_list, samp_list),]

            model_output <- suppressWarnings(update(base_model, data = samp_data))

            rel_coef <- coef(summary(model_output))
            ## this is already the fixed effects matrix, else we pull 'cond'
            if(is.matrix(rel_coef)){
                ret_output <- rel_coef[,1:2, drop = FALSE]
            } else {
                if('cond' %in% names(rel_coef)){
                    ret_output = rel_coef$cond[,1:2, drop = FALSE]
                } else {
                    stop('Other coefficient extraction methods not implemented, please file an issue')
                }
            }
            return(ret_output)
        }
    } else {
        message('Performing case resampling (no random effects)')
        BTCoefEst <- function(){
            samp_data = base_data[sample(nrow(base_data), replace = TRUE),]

            model_output <- suppressWarnings(update(base_model, data = samp_data))

            rel_coef <- coef(summary(model_output))
            ## this should already the fixed effects matrix, else we have an issue
            if(is.matrix(rel_coef)){
                ret_output <- rel_coef[,1:2, drop = FALSE]
            } else {
                stop('Other coefficient extraction methods not implemented, please file an issue')
            }
            return(ret_output)
        }
    }

    ## message('Computing regressions')
    if(requireNamespace('parallel', quietly = TRUE) && num_cores > 1){
        if(suppress_loading_bar || !requireNamespace('pbapply', quietly = TRUE)){
            coef_se_list <- parallel::mclapply(1:resamples, function(i){
                BTCoefEst()}, mc.cores = num_cores, mc.preschedule = FALSE)
        } else {
            coef_se_list <- pbapply::pblapply(1:resamples, function(i){
                BTCoefEst()}, cl = num_cores, mc.preschedule = FALSE)
        }
    } else {
        coef_se_list <- lapply(1:resamples, function(i){
            BTCoefEst()})
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

        if(requireNamespace('parallel', quietly = TRUE) && num_cores > 1){
            if(suppress_loading_bar || !requireNamespace('pbapply', quietly = TRUE)){
                coef_se_list[error_ind] <- parallel::mclapply(1:sum(error_ind), function(i){
                    BTCoefEst()}, mc.cores = num_cores, mc.preschedule = FALSE)
            } else {
                coef_se_list[error_ind] <- pbapply::pblapply(1:sum(error_ind), function(i){
                    BTCoefEst()}, cl = num_cores, mc.preschedule = FALSE)
            }
        } else {
            coef_se_list[error_ind] <- lapply(1:sum(error_ind), function(i){
                BTCoefEst()})
        }

        coef_class <- unlist(lapply(coef_se_list, class))
        error_ind <- coef_class != 'matrix'
    }

    if(return_coefs_instead){
        return(list(base_coef_se = main_coef_se,
                    resampled_coef_se = coef_se_list))
    } else {
        ## We won't dive too far into the world of dfs,
        ## we'll basically default to z-values (Inf df) if not clear
        if('df.residual' %in% names(base_model)){
            orig_df = base_model$df.residual
        } else {
            orig_df = Inf
        }

        res_mat <- BootCI(base_coef_se = main_coef_se,
                          resampled_coef_se = coef_se_list,
                          orig_df = orig_df)

        return(res_mat)
    }
}

#' Calculate number of cores using parallel,
#' or not if parallel not loaded
#' @keywords internal
DetectCores <- function(){
    if('parallel' %in% (.packages())){
        return(parallel::detectCores())
    } else {
        ## 2 so that DetectCores() - 1 gives 1
        ## not that it hugely matters
        message('parallel not loaded, will continue with single core (load parallel or set num_cores manually for parallel work)')
        return(2)
    }
}

#' Calculate Shannon Entropy
#' @keywords internal
CalcEntropy <- function(level_vector){
    freq_as_prob <- table(level_vector) / length(level_vector)

    return(-sum(freq_as_prob * log(freq_as_prob)))
}
