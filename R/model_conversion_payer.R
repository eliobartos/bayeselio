model_conversion_rpp = function(alpha, beta, success, n_sample_conversion, shape, rate, sum_sample, n_post = 1e5) {
  post_conv = model_bernoulli_beta(alpha, beta, success, n_sample_conversion)
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


x = model_conversion_rpp(alpha = 1.641943155, beta = 48.3580568452044, success = 54, n_sample_conversion = 3203,
                         shape = 11, rate = 126.9472414, sum_sample = 574.06)


lapply(x, mean)




bm_d0 = 0.309

mean(x$post_rpu/bm_d0)
quantile(x$post_rpu/bm_d0, prob = c(0.05, 0.95))


mean(x$post_rpu/bm_d0 > 0.6)
