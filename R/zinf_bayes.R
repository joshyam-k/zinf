#' @export
zinf_bayes <- function(x, ...){
  UseMethod("zinf_bayes")
}


#' Create a Bayesian Zero-Inflation Model
#'
#' `zinf_bayes()` is a generic function used to wrap together the two individual
#' models in a zero-inflation model. The function invokes particular methods
#' such as `summary` and `predict` that allow it to be used within typical
#' R modeling workflows
#'
#' Importantly, this generic does no model building itself. The
#' pre-existing rstanarm modeling functions are already so robust that it is
#' sensible to lean on the quality of that software instead of trying to
#' reinvent it within our own function
#'
#' @aliases zinf_bayes
#' @param mody A rstanarm::stan_lmer() object.
#' @param modp A rstanarm::stan_glmer() object.
#' @returns An object of class `zinf_bayes`.

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

  all_grps <- unique(newdata[ , grp_term]) |> pull() |> sort()

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
      .f = ~ full_predict(
        mcmc_y = mod_y,
        mcmc_p = mod_p,
        grp_id = .y,
        fixed_term = fixed_term,
        newdata = .x
      )
    )

  } else if (family_y == "Gamma") {

    all_res <- purrr::map2(
      .x = test_sets,
      .y = all_grps,
      .f = ~ full_predict(
        mcmc_y = mod_y,
        mcmc_p = mod_p,
        grp_id = .y,
        fixed_term = fixed_term,
        newdata = .x,
        family = "Gamma",
        link = link_y
      )
    )

  }

  mean_pred_res <- all_res |>
    purrr::map_dbl(mean)

  point_res <- data.frame(
    group = all_grps,
    post_pred_centers = mean_pred_res
  )

  out <- list(
    posterior_predictive_centers = point_res,
    posterior_predictive_distribution = tibble(
      x = unlist(all_res),
      # this ordering should be correct because of sorting above
      group = sort(rep(all_grps, nrow(mod_y)))
      )
  )

  return(out)

}



