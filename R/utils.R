# Imports
#' @importFrom cli cli_abort



check_formula <- function(object) {
  if(!inherits(object, "formula")) {
      cli_abort(c(
        "{.var object} must be a formula object",
        "x" = "You've supplied a {.cls {class(object)}} object"
      ))
  }
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

check_names_match <- function(object, ref) {

  object <- terms(object)
  factors <- row.names(attr(object, "factors"))
  term_labels <- attr(object, "term.labels")

  mlm <- check_mlm(term_labels)

  if(mlm) {

    non_rf <- factors[!stringr::str_detect(factors, "\\|")]
    rf <- factors[stringr::str_detect(factors, "\\|")]



  } else {
    all_names <- factors
  }




}
