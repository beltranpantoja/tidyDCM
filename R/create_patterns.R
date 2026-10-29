#' Create all possible patterns of the levels passed.
#'
#' @param num_vars number of columns
#' @param levels Elements to be used in the combination. By default `c(0, 1)`,
#'  thus creating a binary matrix.
#' @param column_prefix Prefix for the column names. If column_labels is passed
#'  then this is ignored.
#' @param column_labels Labels for the columns output.
#'
#' @returns a matrix of all possible permutations.
#' @export
#'
create_patterns <- function(
    num_vars,
    levels = c(0, 1),
    column_prefix = "V",
    column_labels = NULL
) {
  # 0 has to be explicitly added if you want it in the matrix
  if (!(0 %in% levels)) {
    warning("0 not included in levels argument when creating the patterns")
  }

  if (typeof(levels) != "double") {
    stop(
      "levels argument must be a double, instead got:", typeof(levels)
    )
  }
  if (length(levels) < 2) {
    stop(
      "levels must contain at least 2 elements"
    )
  }

  # Convert to a list to use the expand.grid
  levels_list <- list(levels)

  patterns_matrix <- as.matrix(
    expand.grid(rep(levels_list, num_vars))
  )


  # If no column labels are passed, we use the prefix to create them.
  if (is.null(column_labels)) {
    column_labels <- paste0(
      column_prefix,
      seq_len(ncol(patterns_matrix))
    )
  }

  colnames(patterns_matrix) <- column_labels

  idx <- order(rowSums(patterns_matrix))
  patterns_matrix[idx, ,drop = FALSE]
}


