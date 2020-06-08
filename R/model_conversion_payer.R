model_conversion_rpp = function(alpha, beta, success, n_sample_conversion, shape, rate, n_sample_rpp, sum_sample, n_post = 1e5) {
  post_conv = model_bernoulli_beta(alpha, beta, success, n_sample_conversion)
  post_rpp = model_exponential_gamma(shape, rate, n_sample_rpp, sum_sample)

  post_rpu = post_conv * post_rpp
  return(
    list(
      post_conv = post_conv,
      post_rpp = post_rpp,
      post_rpu = post_rpu
      )
    )
}


x = model_conversion_rpp(alpha = 2, beta = 48, success = 67, n_sample_conversion = 873,
                         shape = 11, rate = 100, n_sample_rpp = 67, sum_sample = 725.89)


lapply(x, mean)

bm_d0 = 0.309

mean(x$post_rpu/bm_d0)
