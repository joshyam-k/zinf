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

  check_family(family_y, family_p)

  out <- list(
    mods = list(mody, modp),
    call = funcCall
  )

  class(out) <- "zinf_bayes"
  out

}

#' @export
summary.zinf_bayes <- function(object, ...) {

  out <- list(
    s1 = summary(object$mods[[1]]),
    s2 = summary(object$mods[[2]])
  )

  class(out) <- "summary.zinf_bayes"
  out
}

#' @export
print.summary.zinf_bayes <- function(x, ...){

  cat("\nY MODEL: \n")
  print(x$s1)
  cat("\nZ MODEL: \n")
  print(x$s2)
  invisible(x)

}


predict.zinf_bayes <- function(object, newdata, ...) {
  mod_y <- as.data.frame(object$mods[[1]])
  mod_p <- as.data.frame(object$mods[[2]])

  family_y <- object$mods[[1]]$family$family
  link_y <- object$mods[[1]]$family$link

  # need to know number of groups to predict on


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
#m <- zinf_bayes(t, p)


as.data.frame(p) |> head()



mod <- lme4::lmer(mpg > 16 ~ wt + (1 | cyl), mtcars)

predict(mod, mtcars[mtcars$cyl == 8, ])
