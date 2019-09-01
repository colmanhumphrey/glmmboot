#' Simulated data containing three fixed effects and one random effect
#'
#' A small normal dataset with a proportional
#' outcome to illustrate the use of this package.
#' The outcome has mean
#' \code{expit(0.5 + 0.1 * x_var1 + 0.2 * x_var2 + 0.3 * x_var3 + SUBJ_VAL)}
#' where SUBJ_VAL are the values of the random effect
#' The SD of y is then shrunk by 0.9 relative to a binomial distribution,
#' and then beta values are generated. Arbitrarily close to the endpoints
#' gives zeros and ones.
#'
#' @format A data frame with 300 rows and 4 variables:
#' \describe{
#'   \item{x_var1}{independent normally distributed variable}
#'   \item{x_var2}{independent normally distributed variable}
#'   \item{x_var3}{independent normally distributed variable}
#'   \item{subj}{levels of random effect}
#'   \item{y}{outcome: lives in interval [0,1]}
#' }
#'
"test_data"
