#' @export

zinf_freq <- function(x, ...){
  UseMethod("zinf_freq")
}

#' @export
#' @rawNamespace export(zinf_freq.default)

zinf_freq.default <- function(formula, data, ...) {

  funcCall <- match.call(expand.dots = T)
  check_formula(formula)
  check_names_match(formula, names(data))

  lme4::lmer(formula)

}


