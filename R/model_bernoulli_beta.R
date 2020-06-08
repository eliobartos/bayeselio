#' Run bayesian beta - bernoulli model for estimating proportion of a bernoulli variable
#'
#' Runs a bayesian estimation of proportion using bernoulli distribution as likelihood and beta distribution as conjugate prior.
#' Posterior distribution is beta distribution.
#' Prior used is Beta(alpha + 1, beta + 1)
#'
#' @param alpha Parameter for prior distribution representing the number of success
#' @param beta Parameter for prior distribution representing the number of fails
#'
#' @param success Number of success cases in your data
#' @param n_sample Total number of cases in your data
#' @param n_post Size of sample from posterior distribution
#'
#' @return Vector of samples from posterior distribution
#'
#' Posterior distribution is Beta(alpha + 1 + success, beta + 1 + total - success)
#'
#' @examples
#' post = model_bernoulli_beta(0, 0, 20, 100) # No prior information, pror is uniform
#' post2 = model_bernoulli_beta(5, 95, 3, 50) # Prior succes rate is around 5% with estimation strenght as it was estimated on a sample of 100
#'
#' mean(post)
#' quantile(post, probs = c(0.05, 0.95)) # 90% highest density posterior interval
#'
#' mean(post2)
#' quantile(post2, probs = c(0.05, 0.95))
model_bernoulli_beta = function(alpha, beta, success, n_sample, n_post = 1e5) {
  rbeta(n_post, alpha + success, beta + n_sample - success)
}

