#' Run bayesian exponential-gamma model for estimating non-zero positive variable
#'
#' Runs a bayesian estimation of non-zero positive variable using exponential distribution as likelihood and gamma distribution as conjugate prior.
#' Posterior distribution is gamma distribution.
#' Prior used is Gamma(shape, rate)
#'
#' Mean of prior distribution is shape/rate. Remember Exp(lambda) has mean of 1/lambda.
#'
#' @param shape Parameter for prior distribution representing the number of samples from before - 1
#' @param rate Parameter for prior distribution representing the sum of samples from before
#'
#' @param n_sample Total number of cases in your data
#' @param sum_sample Sum of variable for each successful case.
#' @param n_post Size of sample from posterior distribution
#'
#' @return Vector of samples from posterior distribution representing mean of exponential distribution (1/lambda).
#'
#' Posterior distribution is Gamma(shape + n_sample, rate + sum_sample)
#'
#' @examples
#' post = model_exponential_gamma(0, 0, 20, 100) # No prior information, pror is uniform
#' post2 = model_exponential_gamma(5, 95, 3, 50) # Prior succes rate is around 5% with estimation strenght as it was estimated on a sample of 100
#'
#' mean(post)
#' quantile(post, probs = c(0.05, 0.95)) # 90% highest density posterior interval
#'
#' mean(post2)
#' quantile(post2, probs = c(0.05, 0.95))
model_exponential_gamma = function(shape, rate, n_sample, sum_sample, n_post = 1e5) {
  1/rgamma(n_post, shape + n_sample, rate + sum_sample)
}

