#' Convert a data frame to a numeric matrix
#'
#' Drops all non-numeric columns and converts the rest to a matrix. If the
#' first column is non-numeric (character, factor, etc.) and its values are
#' unique and non-missing, it is used as the row names. Numeric first columns
#' are always treated as data.
#'
#' @param x A data frame or tibble.
#' @param ... Not used. Must be empty.
#' @returns A numeric matrix.
#' @export
to_matrix <- function(x, ...) {
  UseMethod("to_matrix")
}

#' @export
to_matrix.data.frame <- function(x, ...) {
  rlang::check_dots_empty()

  is_num <- vapply(x, is.numeric, logical(1))
  out <- as.matrix(x[is_num])

  if (ncol(x) > 0 && !is_num[[1]]) {
    first_col <- x[[1]]
    if (!anyNA(first_col) && !anyDuplicated(first_col)) {
      rownames(out) <- as.character(first_col)
    }
  }

  out
}
