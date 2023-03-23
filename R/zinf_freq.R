#' @export
zinf_freq <- function(x, ...){
  UseMethod("zinf_freq")
}

#' @export
#' @rawNamespace export(zinf_freq.formula)

zinf_freq.formula <- function(formula_lm, formula_logreg, data, ...) {

  funcCall <- match.call(expand.dots = T)
  model_formulas <- list(formula_lm, formula_logreg)
  purrr::map(
    model_formulas,
    ~ validate_args(.x, data)
    )
  purrr::map(
    model_formulas,
    ~ check_names_match(.x, names(data))
    )

  response <- formula_lm[[2]]

  nz_data <- data |>
    dplyr::filter(!!response > 0)

  mod_y <- lme4::lmer(formula_lm, nz_data)
  mod_z <- lme4::glmer(formula_logreg, data, family = "binomial")

  out <- list(
    lm = mod_y,
    logreg = mod_z,
    call = funcCall
  )

  class(out) <- "zinf_freq"
  out

}


