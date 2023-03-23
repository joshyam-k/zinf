#' @export
zinf_freq <- function(x, ...){
  UseMethod("zinf_freq")
}

#' @export
#' @rawNamespace export(zinf_freq.formula)

zinf_freq.formula <- function(formula_lm, formula_logreg, data, ...) {

  funcCall <- match.call(expand.dots = T)
  check_names_match(formula_lm, formula_logreg, names(data))
  validate_args(formula_lm, formula_logreg, data)

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

#' @export
summary.zinf_freq <- function(object, ...) {
  out <- list(
    call = object$call,
    lm = object$lm,
    logreg = object$logreg
  )

  class(out) <- "summary.zinf_freq"
  out
}

#' @export
print.summary.zinf_freq <- function(object, ...) {

  summary_lm <- summary(object$lm)
  summary_logreg <- summary(object$logreg)
  both_summary <- list(
    "---------------Y Mod---------------",
    summary_lm,
    "---------------Z Mod---------------",
    summary_logreg
    )
  purrr::walk(both_summary, print)

}


