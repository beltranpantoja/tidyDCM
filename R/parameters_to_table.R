#' Convert a tibble of test parameters to a wide format tibble
#'
#' @param item_parameters A [tibble::tibble()] or a `matrix` with the item parameters of the DCM.
#' @param fill_NA Should the NA values of the table be filled with zeroes?
#'  Default is `FALSE`.
#' @param qmatrix The Q-matrix associated with the item coefficients. It can
#'  be passed to check `item_parameters` are correctly constructed
#' @param term_sep A string to be used as separator for the interaction effects
#'  names. Default is "."
#' @param term_prefix A string to be used as prefix for the term names.
#'
#'
#' @returns A [tibble::tibble()] with the item parameters in a long format
#' @export
#'
#'
parameters_to_table <- function(
    item_parameters,
    fill_NA = FALSE,
    qmatrix = NULL,
    term_sep = "-",
    term_prefix = ""
) {

  # TODO: implement some sort of parameter check
  # We get the names of the attributes either from the item parameters
  if (is.null(qmatrix)) {
    qmatrix <- get_qmatrix(item_parameters)

  }

  num_attrs <- ncol(qmatrix) - 1

  # This creates all possible interactions in the standard order
  all_levels <- c(
    "0",
    unlist(lapply(seq_len(num_attrs), function(k) {
      apply(combn(seq_len(num_attrs), k), 2, paste, collapse = term_sep)
    }))
  )

  # We convert the attribute information into terms and then
  # use that to pivot wider.
  # The all_levels handles that the order is correct.
  item_parameters |>
    mutate(
      term = factor(.data$term, levels = all_levels)
    ) |>
    select(all_of(c("item", "estimate", "term"))) |>
    tidyr::pivot_wider(
      names_from = "term",
      values_from = "estimate",
      names_sort = TRUE,
      names_prefix = term_prefix,
      values_fill = if (fill_NA) 0 else NA,
      names_expand = TRUE
    )
}
