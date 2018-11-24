#' Generates block resamples
#'
#' Takes in a list of unique levels in the random columns,
#' gives back a random sampling of each.
#'
#' @keywords internal
GenSample <- function(list_of_levels,
                      rand_columns = NULL,
                      unique_resample_lim = NULL,
                      reduce_by_one = TRUE){
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
                      if(reduce_by_one){
                          samp = sample(levs, size = length(levs) - 1, replace = TRUE)
                      } else {
                          samp = sample(levs, replace = TRUE)
                      }
                      len_uni = length(unique(samp))
                  }
        } else {
            if(reduce_by_one){
                samp = sample(levs, size = length(levs) - 1, replace = TRUE)
            } else {
                samp = sample(levs, replace = TRUE)
            }
        }
        return(samp)
    }

    temp = lapply(rand_columns, function(rc){gen_samp_lev(list_of_levels[[rc]], unique_resample_lim[rc])})
    names(temp) = rand_columns
    return(temp)
}

#' this function takes in original vectors, and resampled editions,
#' it spits back the matching index of the original variables for the new resampled ones
#'
#' @keywords internal
GenResamplingIndex <- function(orig_list,
                               sampled_list){
    if(length(orig_list) != length(sampled_list)){
        stop('lists must be the same length (the original variables and the sampled variables)')
    }
    
    orig_strings <- do.call(paste, orig_list)
    sampled_strings <- do.call(paste, expand.grid(sampled_list))

    ## don't want to call parallel version, since this function
    ## is called within parallel func.
    all_ind_list <- lapply(sampled_strings, function(str){
        which(orig_strings == str)})

    all_ind = unlist(all_ind_list)

    return(all_ind)
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
#' @export
#'
#' @return A vector of the variables that
#' are treated as random
#'
#' @examples
#' GetRand('y ~ age + (1 | school)')
#' GetRand('y ~ income + (1 | school) + (1 | school:section)')
#' GetRand('y ~ income + (1 | school) + (1 | school/section)')
#' GetRand(as.formula('y ~ x + (1 | z)'))
#' GetRand('y ~ x')
GetRand <- function(form_with_bars){
    findbar_list <- FindBars(form_with_bars)
    
    first_pass <- unlist(
        lapply(findbar_list,
               function(x){
                   if(class(x) == 'call'){
                       return(as.character(x[3]))
                   } else {
                       first_bar <- unlist(gregexpr('|', x, fixed = TRUE))[[1]]
                       return(trimws(
                           substr(x, start = first_bar + 1, stop = nchar(x))))
                   }
               }))
    ## potentially we'll have : and so on
    all_vars <- all.vars(as.formula(form_with_bars))

    in_firstpass <- unlist(lapply(all_vars, function(y){
        any_match = unlist(lapply(first_pass, function(x){
            grepl(y, x, fixed = TRUE)}))
        return(sum(any_match) > 0)}))

    return(all_vars[in_firstpass])
}

#' returns the terms with bars
#'
#' @inheritParams GetRand
#'
#' @keywords internal
FindBars <- function(form_with_bars){
    ## if it's just text right now, convert it
    if(!('formula' %in% class(form_with_bars))){
        form_with_bars <- as.formula(form_with_bars)
    }

    ## get the terms
    form_terms <- attributes(terms(form_with_bars))$term.labels

    ## return the terms with a bar
    return(as.list(form_terms[grepl('|', form_terms, fixed = TRUE)]))
}
    
