#' Create a tibble  of items and their parameters
#'
#'
#' @param qmatrix Q-matrix of the test
#' @param ... values with which to fill the respective order terms. i.e. the first
#'  number is assumed to be the intercept, then main effects, then two-way
#'  interaction and so on. If it's shorter than the number of parameters, then
#'  those get filled with 0s.
#' @inheritParams items_from_qmatrix rule link_fun
#'
#' @returns A tibble of type <[`item-parameters`][args_item_parameters]>
#' @export
#' @examples
#' qmat <- matrix(c(
#'  1, 0, 0,
#'  1, 0, 0,
#'  0, 1, 0,
#'  0, 1, 0,
#'  0, 0, 1,
#'  0, 0, 1,
#'  1, 1, 0,
#'  0, 1, 1
#' ), byrow = TRUE, ncol=3)
#'
#'
#' item_parameters <- generate_items(qmat, .2, .6, rule="ACDM")
#' sample <- generate_examinees(1000, 3, .5)
#'
#' generate_responses(item_parameters, sample)
generate_items <- function(
    qmatrix,
    ...,
    rule = NULL,
    link_fun = NULL
    ) {

  values <- c(...)

  base_tibble <- items_from_qmatrix(qmatrix, rule, link_fun)

  order_int <- as.integer(base_tibble$order)
  max_order <- max(order_int, na.rm = TRUE)

  if (length(values) > max_order + 1) {
    stop(
      "`values` has length ", length(values),
      " but must be at most ", max_order + 1,
      " (one per order, 0 through ", max_order, ").",
      call. = FALSE
    )
  }


  # The values and a 0 by default
  lookup <- c(values, 0)
  # This converts the order to an index. If larger than the lookup, then
  # it just takes the last one.
  idx <- pmin(order_int + 1L, length(lookup))

  base_tibble |>
    dplyr::mutate(estimate = lookup[idx])
}
