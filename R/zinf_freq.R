#' @export
zinf_freq <- function(x, ...){
  UseMethod("zinf_freq")
}

#' Create a Frequentist Zero-Inflation Model
#'
#' `zinf_freq()` is a generic function used to wrap together the two individual
#' models in a zero-inflation model. The function invokes particular methods
#' such as `summary` and `predict` that allow it to be used within typical
#' R modeling workflows
#'
#' Importantly, this generic does no model building itself. The
#' pre-existing lme4 modeling functions are already so robust that it is
#' sensible to lean on the quality of that software instead of trying to
#' reinvent it within our own function
#'
#' @aliases zinf_freq
#' @param mody A lme4::lmer() object.
#' @param modp A lme4::glmer() object.
#' @returns An object of class `zinf_freq`.
#'
#' @export
#' @rawNamespace export(zinf_freq.merMod)
zinf_freq.merMod <- function(mody, modp, ...) {

  funcCall <- match.call(expand.dots = T)

  validate_models_f(mody, modp)

  out <- list(
    mods = list(mody, modp),
    call = funcCall
  )

  class(out) <- "zinf_freq"
  out

}


#' @export
summary.zinf_freq <- function(object, ...) {

  out <- list(
    s1 = summary(object$mods[[1]]),
    s2 = summary(object$mods[[2]])
  )

  class(out) <- "summary.zinf_freq"
  out
}

#' @export
print.summary.zinf_freq <- function(x, ...) {

  cat("--------\nY MODEL: \n--------\n")
  print(x$s1)
  cat("\n\n--------\nZ MODEL: \n-------- \n")
  print(x$s2)
  invisible(x)

}

#' @export
predict.zinf_freq <- function(object, newdata = NULL, ...) {
  lm <- object$mods[[1]]
  logreg <- object$mods[[2]]
  pred_lm <- predict(lm, newdata = newdata, type = "response")
  pred_logreg <- predict(logreg, newdata = newdata, type = "response")
  pred <- pred_lm * pred_logreg

  # need to compute means by the grouping variable
  grp_var <- names(object$mods[[1]]@flist)
  sym_grp_var <- rlang::sym(grp_var)

  bind <- newdata |>
    dplyr::mutate(preds = pred) |>
    dplyr::group_by(!!sym_grp_var) |>
    dplyr::summarise(prediction = mean(preds))

  return(bind)
}



# y <- lme4::lmer(mpg ~ wt + (1 | cyl), mtcars)
# p <- lme4::glmer(mpg > 16 ~ wt + (1 | cyl), family = binomial, mtcars)
#
# mod <- zinf_freq(y, p)
