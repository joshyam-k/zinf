#' @export
zinf_freq <- function(x, ...){
  UseMethod("zinf_freq")
}

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
