# Imports
#' @importFrom cli cli_abort

validate_args <- function(f1, f2, x) {

  args_val <- function(f, x) {
    if(attr(terms(f), "response") == 0)
      cli_abort(c(
        "x" = "Formula object must include a response variable"
      ))
    if(!is.data.frame(x))
      cli_abort(c(
        "x" = "argument `data` must be of class 'data.frame'"
      ))
  }

  purrr::walk(c(f1, f2), ~ args_val(.x, x))

}

check_mlm <- function(all_terms) {

  by_term <- function(term) {
    if(stringr::str_detect(term, "\\|")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  out <- sum(purrr::map_dbl(all_terms, by_term))

  return(ifelse(out > 0, TRUE, FALSE))

}

extract_var_from_rf <- function(all_terms) {

  by_term <- function(term) {
    term <- stringr::str_remove(term, "1\\s?\\|") |>
      stringr::str_trim(side = "both")

    return(term)
  }

  out <- purrr::map_chr(all_terms, by_term)
  return(out)

}

check_names_match <- function(f1, f2, ref) {

  name_match <- function(object, ref) {
    object <- terms(object)
    factors <- row.names(attr(object, "factors"))
    term_labels <- attr(object, "term.labels")

    mlm <- check_mlm(term_labels)

    if(mlm) {

      non_reffect_terms <- factors[!stringr::str_detect(factors, "\\|")]
      reffect <- factors[stringr::str_detect(factors, "\\|")]
      reffect_terms <- extract_var_from_rf(reffect)

      all_names <- c(non_reffect_terms, reffect_terms)

    } else {
      all_names <- factors
    }

    if(!all(all_names %in% ref)) {
      cli_abort(c(
        "x" = "All names specified in the formula must also exist in the input dataset"
      ))
    }
  }

  purrr::walk(c(f1, f2), ~ name_match(.x, ref))


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
    stringr::str_subset(as.character(grp_id))

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
    stringr::str_subset(as.character(grp_id))

  mat_mcmc_sub <- mcmc |>
    dplyr::select(tidyselect::all_of(c("(Intercept)", fixed_term, rf))) |>
    t()

  mcmc_sigma <- mcmc |>
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

  mat_sim <- mapply(sim_normal, x = mat_mult, sig = mcmc_sigma) |>
    matrix(ncol = nrow(mcmc))

  return(mat_sim)


}

full_gaussian_predict <- function(mcmc_y, mcmc_p, grp_id, fixed_term, newdata) {
  p_component <- log_reg_predict(mcmc_p, grp_id, fixed_term, newdata)
  y_component <- normal_predict(mcmc_y, grp_id, fixed_term, newdata)

  if(!all(dim(p_component) == dim(y_component))) {
    cli_abort(c(
      "x" = "Problem with matrix mult"
    ))
  }

  full <- y_component * p_component

  post_pred <- apply(full, 2, mean)

  return(post_pred)





}
