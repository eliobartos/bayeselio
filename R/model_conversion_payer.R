#' Bayesian model that combines conversion estimation and value estimation for converted cases
#'
#' Model uses total number of cases and successful cases to estimate conversion based on bernoulli-beta model
#' and for successful cases estimates continuous variable using exponential-gamma model
#' 
#' Final result is estimated value for each case (not just converted cases). Motivation is when you have some percent of
#' users that spend money and others that spend no money. First we estimate conversion to payers rate, and for payers estimate
#' revenue per payer (rpp). Final posterior distribution is multiple of these two posterior distributions and is representing
#' revenue per user (rpu). 
#' 
#' Check model_bernoulli_beta and model_exponential_gamma for details regarding each model.
#' 
#' @param alpha Parameter for prior in bernoulli-beta model representing number of success - 1
#' @param beta Parameter for prior in bernoulli-beta model representing number of fails - 1
#' @param success Actual number of successful cases (payers) in your data.
#' @param total_sample_size Total number of cases in your data.
#' 
#' @param shape Parameter for prior in exponential-gamma model representing the number of samples from before - 1
#' @param rate Parameter for prior in exponential-gamma model representing the sum of samples from before
#' 
#' @param sum_sample Sum of variable (money) for each successful case.
#' @param n_post Size of sample from posterior distribution
#' 
#' @return List of 3 posterior distributions: post_conv representing conversion rate (success rate) from bernoulli-beta model,
#'  post_rpp representing value of variable (money) per successful case from exponential-gamma model, post_rpu representing value
#'  of variable per each case (money per all cases), post_rpu = post_conv * post_rpp
#' 
#' @author Elio Bartoš
#' 
#' @example 
#' # If we had 20 users out of which 5 spent in total of 10$.
#' # If beta prior we input success rate of 1/10, on a sample size of 8 observations
#' # In gamma prior we input 1$ revenue per successful case on a sample of 4 cases
#' x = model_conversion_rpp(alpha = 1, beta = 9, success = 5, total_sample_size = 20,
#'                         shape = 11, rate = 5, sum_sample = 10)
#' 
#' lapply(x, mean)
#' quantile(x$post_rpu, prob = c(0.04, 0.96)) # 92% posterior density interval

model_conversion_rpp = function(alpha, beta, success, total_sample_size, shape, rate, sum_sample, n_post = 1e5) {
  post_conv = model_bernoulli_beta(alpha, beta, success, total_sample_size)
  post_rpp = model_exponential_gamma(shape, rate, success, sum_sample)

  post_rpu = post_conv * post_rpp
  return(
    list(
      post_conv = post_conv,
      post_rpp = post_rpp,
      post_rpu = post_rpu
      )
    )
}
