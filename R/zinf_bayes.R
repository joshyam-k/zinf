#' @export
zinf_bayes <- function(x, ...){
  UseMethod("zinf_bayes")
}

#' @export
#' @rawNamespace export(zinf_bayes.stanreg)
zinf_bayes.stanreg <- function(mody, modp, ...){

  funcCall <- match.call(expand.dots = T)

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
  cat("--------\nY MODEL: \n--------")
  print(x$s1)
  cat("\n\n--------\nZ MODEL: \n--------")
  print(x$s2)
  invisible(x)

}

#' @export
predict.zinf_bayes <- function(object, newdata, ...) {


  mod_y <- as.data.frame(object$mods[[1]])
  mod_p <- as.data.frame(object$mods[[2]])

  family_y <- object$mods[[1]]$family$family
  link_y <- object$mods[[1]]$family$link

  # need to know number of groups to predict on
  mod_terms <- attr(terms(object$mods[[1]]$formula), "term.labels")
  rand_id <- stringr::str_which(mod_terms, "\\|")
  grp_term <- stringr::str_extract(mod_terms[rand_id], "(?<=(\\|)).*") |>
    stringr::str_trim(side = "both")

  all_grps <- unique(newdata[ , grp_term])

  fixed_term <- mod_terms[-rand_id]

  # add a check to see if newdata contains all the necessary variables
  needed <- c(grp_term, fixed_term)
  validate_newdata(newdata, needed)

  # separate out the test set by the rf variable
  test_sets <- all_grps |>
    purrr:::map(.f = ~ extract_test_sets(
      id = .x, x = newdata,
      grp_term = grp_term,
      fixed_term = fixed_term
      ))

  # iterate over test_sets
  if(family_y == "gaussian") {

    all_res <- purrr::map2(
      .x = test_sets,
      .y = all_grps,
      .f = ~ full_gaussian_predict(
        mcmc_y = mod_y,
        mcmc_p = mod_p,
        grp_id = .y,
        fixed_term = fixed_term,
        newdata = .x
      )
    )

  }

  return(all_res)

}



# ex ---------
# library(rstanarm)
# t <- stan_lmer(
#   Y ~ X + (1 | rfid),
#   data = df,
#   prior_intercept = normal(40, 10),
#   prior = normal(3, 1),
#   prior_aux = exponential(1),
#   prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
#   chains = 4, iter = 5000, seed = 84735,
#   cores = parallel::detectCores()
# )
#
# p <- stan_glmer(
#   Z ~X + (1 | rfid),
#   data = df,
#   family = binomial,
#   prior_intercept = normal(40, 10),
#   prior = normal(3, 1),
#   prior_aux = exponential(1),
#   prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
#   chains = 4, iter = 5000, seed = 84735,
#   cores = parallel::detectCores()
# )
#
# test <- population |>
#    filter(group == 1) %>%
#   rename(rfid = group)
#
# m <- zinf_bayes(t, p)

# p <- predict(m, test)
#
# tibble(
#   x = p$rfid
# ) %>%
#   ggplot(aes(x = x)) +
#   geom_density() +
#   geom_vline(xintercept = 27)


