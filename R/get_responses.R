#' Extract the response data of a DCM object
#'
#' @param object Either a fitted model of type [CDM::gdina]/[GDINA::GDINA]
#'
#' @returns the requested data in a tibble.
#' @export
get_responses <- function(object) {
  UseMethod("get_responses")
}

#' @export
get_responses.GDINA <- function(object) {
  dat <- object$options$dat

  # We set the column names if it doesnt have any
  if (is.null(colnames(dat))) {
    colnames(dat) <- paste0("V", seq_len(ncol(dat)))
  }

  dat |>
    as_tibble()

}

#' @export
get_responses.gdina <- function(object) {
  object$data |>
    as_tibble()
}



