#' @export
zinf_bayes <- function(x, ...){
  UseMethod("zinf_bayes")
}

#' @export
#' @rawNamespace export(zinf_bayes.stanreg)

zinf_bayes.stanreg <- function(mody, modp){

  funcCall <- match.call(expand.dots = T)
  # some check that the response is all nonzero in mody$data

  # checking model families

  family_y <- mody$family$family
  family_p <- modp$family$family
  link_y <- mody$family$link
  pass_y <- c(family_y, link_y)

  check_family(family_y, family_p)

  mcmc_out_y <- as.data.frame(mody)
  mcmc_out_p <- as.data.frame(modp)

  out <- list(
    mcmc_y = mcmc_out_y,
    mcmc_p = mcmc_out_p,
    mod_info_y = pass_y,
    call = funcCall
  )

  class(out) <- "zinf_bayes"
  out

}



# ex ---------
# library(rstanarm)
# t <- stan_lmer(
#   mpg ~ wt + (1 | cyl),
#   data = mtcars,
#   prior_intercept = normal(40, 10),
#   prior = normal(3, 1),
#   prior_aux = exponential(1),
#   prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
#   chains = 4, iter = 5000, seed = 84735,
#   cores = parallel::detectCores()
# )
#
# p <- stan_glmer(
#   mpg > 16 ~ wt + (1 | cyl),
#   data = mtcars,
#   family = binomial,
#   prior_intercept = normal(40, 10),
#   prior = normal(3, 1),
#   prior_aux = exponential(1),
#   prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
#   chains = 4, iter = 5000, seed = 84735,
#   cores = parallel::detectCores()
# )
# #
zinf_bayes(t, p)

