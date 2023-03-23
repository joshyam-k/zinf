# Imports
#' @importFrom cli cli_abort

validate_args <- function(f, x) {
  if(attr(terms(f), "response") == 0)
    cli_abort(c(
      "x" = "Formula object must include a response variable"
    ))
  if(!is.data.frame(x))
    cli_abort(c(
      "x" = "argument `data` must be of class 'data.frame'"
    ))
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

check_names_match <- function(object, ref) {

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
