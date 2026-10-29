#' Transforms a Q-matrix like matrix to the tibble format
#'
#' @param x An object (matrix, data.frame, or tibble)
#' @param ... Additional arguments passed to methods
#'
#' @returns A tibble (or the original data.frame/tibble if passed one)
#' @export
as_qmatrix <- function(x, ...) {
  UseMethod("as_qmatrix")
}

#' @rdname as_qmatrix
#' @param col_prefix Prefix for the automatically generated columns if they don't have names.
#' @export
as_qmatrix.matrix <- function(x, col_prefix = "Attr", ...) {
  x_mat <- x

  # Check for row names; if missing, create item names ("item1", "item2", etc.)
  item_names <- rownames(x_mat)
  if (is.null(item_names) || all(item_names == "")) {
    item_names <- paste0("item", seq_len(nrow(x_mat)))
  }

  # Check for column names; if missing, assign default labels with prefix
  if (is.null(colnames(x_mat))) {
    col_names <- paste0(col_prefix, seq_len(ncol(x_mat)))
    colnames(x_mat) <- col_names
  }

  # Convert to tibble and prepend the item column
  res <- tibble::as_tibble(x_mat)
  res <- tibble::add_column(res, item = item_names, .before = 1)

  return(res)
}

#' @rdname as_qmatrix
#' @export
as_qmatrix.data.frame <- function(x, ...) {
  return(x)
}




#' Transforms a matrix of profiles into a tibble format.
#'
#' @param x An object (matrix, data.frame, or tibble)
#' @param ... Additional arguments passed to methods
#'
#' @returns A tibble (or the original data.frame/tibble if passed one)
#' @export
as_examinees <- function(x, ...) {
  UseMethod("as_examinees")
}

#' @rdname as_examinees
#' @param col_prefix Prefix for the automatically generated columns if they don't have names.
#' @export
as_examinees.matrix <- function(x, col_prefix = "Attr", ...) {
  x_mat <- x

  # Check for column names; if missing, assign default labels with prefix
  if (is.null(colnames(x_mat))) {
    col_names <- paste0(col_prefix, seq_len(ncol(x_mat)))
    colnames(x_mat) <- col_names
  }

  # Convert to tibble
  tibble::as_tibble(x_mat)
}

#' @rdname as_examinees
#' @export
as_examinees.data.frame <- function(x, ...) {
  return(x)
}
