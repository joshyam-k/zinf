#' @export
zinf_freq <- function(formula, data, ...){
  UseMethod("zinf_freq")
}

#' @export
#' @rawNamespace export(zinf_freq.formula)

zinf_freq.formula <- function(formula, data, ...) {

  funcCall <- match.call(expand.dots = T)
  check_formula(formula)
  check_names_match(formula, names(data))

  #lme4::lmer(formula)

}


