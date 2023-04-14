# Imports
#' @importFrom cli cli_abort

validate_models_f <- function(m1, m2) {
  if(class(m1) != "lmerMod")
    cli_abort(c(
      "x" = "zinf_freq models require the first model to be of class lmerMod"
    ))
  if(class(m2) != "glmerMod")
    cli_abort(c(
      "x" = "zinf_freq models require the second model to be of class glmerMod"
    ))
}


check_family <- function(o1, o2) {
  if(!(o1 %in% c("Gamma", "gaussian"))){
    cli_abort(c(
      "x" = "Support only exists for modelling the non-zero response using a Gamma or Gaussian glm"
    ))
  }

  if(!(o2 %in% c("binomial"))) {
    cli_abort(c(
      "x" = "Support only exists for a logistic regression classification model"
    ))
  }
}

extract_test_sets <- function(id, x, grp_term, fixed_term) {
  sub <- x[x[ , grp_term] == id, ]
  sub <- sub |>
    dplyr::select(all_of(c(fixed_term, grp_term)))
  return(sub)
}

validate_newdata <- function(x, names_needed) {
  if(!all(names_needed %in% names(x))){
    cli_abort(c(
      "x" = "new data must contain all of the variables used to build the model"
    ))
  }
}


log_reg_predict <- function(mcmc, grp_id, fixed_term, newdata) {

  # subsetting mcmc df
  rf <- names(mcmc) |>
    stringr::str_subset("b\\[") |>
    stringr::str_subset(paste0(":", as.character(grp_id), "]"))

  mat_mcmc_sub <- mcmc |>
    dplyr::select(tidyselect::all_of(c("(Intercept)", fixed_term, rf))) |>
    t()

  # remove rownames for this step
  rownames(newdata) <- NULL

  mat_newdata <- newdata |>
    dplyr::select(tidyselect::all_of(fixed_term)) |>
    dplyr::mutate(intercept = 1, rf = 1) |>
    dplyr::select(intercept, tidyselect::all_of(fixed_term), rf) |>
    as.matrix()

  mat_mult <- mat_newdata %*% mat_mcmc_sub

  sim_bernoulli <- function(x) {
    link <- 1/(1 + exp(-x))
    rbinom(1, 1, link)
  }

  mat_sim <- apply(mat_mult, c(1, 2), sim_bernoulli)

  return(mat_sim)

}

normal_predict <- function(mcmc, grp_id, fixed_term, newdata) {

  rf <- names(mcmc) |>
    stringr::str_subset("b\\[") |>
    stringr::str_subset(paste0(":", as.character(grp_id), "]"))

  mat_mcmc_sub <- mcmc |>
    dplyr::select(tidyselect::all_of(c("(Intercept)", fixed_term, rf))) |>
    t()

  mat_sigma <- mcmc |>
    dplyr::select("sigma") |>
    as.matrix()

  rownames(newdata) <- NULL

  mat_newdata <- newdata |>
    dplyr::select(tidyselect::all_of(fixed_term)) |>
    dplyr::mutate(intercept = 1, rf = 1) |>
    dplyr::select(intercept, tidyselect::all_of(fixed_term), rf) |>
    as.matrix()

  mat_mult <- mat_newdata %*% mat_mcmc_sub

  sim_normal <- function(x, sig) {
    rnorm(1, mean = x, sd = sig)
  }

  mat_sim <- mapply(sim_normal, x = mat_mult, sig = mat_sigma) |>
    matrix(ncol = nrow(mcmc))

  return(mat_sim)


}


gamma_predict <- function(mcmc, grp_id, fixed_term, newdata, link = "log") {

  rf <- names(mcmc) |>
    stringr::str_subset("b\\[") |>
    stringr::str_subset(paste0(":", as.character(grp_id), "]"))

  mat_mcmc_sub <- mcmc |>
    dplyr::select(tidyselect::all_of(c("(Intercept)", fixed_term, rf))) |>
    t()

  mat_alpha <- mcmc |>
    dplyr::select("alpha") |>
    as.matrix()

  rownames(newdata) <- NULL

  mat_newdata <- newdata |>
    dplyr::select(tidyselect::all_of(fixed_term)) |>
    dplyr::mutate(intercept = 1, rf = 1) |>
    dplyr::select(intercept, tidyselect::all_of(fixed_term), rf) |>
    as.matrix()

  mat_mult <- mat_newdata %*% mat_mcmc_sub


  if (link == "log") {
    mat_mult <- log(mat_mult)
  } else if (link == "inverse") {
    mat_mult <- -1/mat_mult
  } else if (link == "identity") {
    mat_mult <- mat_mult
  } else {
    cli_abort(c(
      "x" = "Only `log`, `inverse`, and `identity` link functions are currently supported for the Gamma glm"
    ))
  }

  sim_gamma <- function(x, alpha) {
    rgamma(1, shape = alpha, rate = alpha/x)
  }

  mat_sim <- mapply(sim_gamma, x = mat_mult, alpha = mat_alpha) |>
    matrix(ncol = nrow(mcmc))

  return(mat_sim)

}


full_predict <- function(mcmc_y, mcmc_p, grp_id, fixed_term, newdata, family = "gaussian", link = "identity") {
  p_component <- log_reg_predict(mcmc_p, grp_id, fixed_term, newdata)

  if (family == "gaussian") {
    y_component <- normal_predict(mcmc_y, grp_id, fixed_term, newdata)
  } else if (family == "Gamma") {
    y_component <- gamma_predict(mcmc_y, grp_id, fixed_term, newdata, link = link)
  }

  if(!all(dim(p_component) == dim(y_component))) {
    cli_abort(c(
      "x" = "Problem with matrix mult"
    ))
  }

  full <- y_component * p_component

  post_pred <- apply(full, 2, mean)

  return(post_pred)


}
