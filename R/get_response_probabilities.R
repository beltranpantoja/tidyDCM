#' Generate response probabilities
#'
#' Returns a tibble with the probability of each type of response in an item
#' by item and profile. The probability value is \eqn{P(X=h_i | \alpha_l)} where
#' \eqn{h_i} is response h in item i and \eqn{\alpha_l} is the profile. For now
#' it only accepts dichotomous items.
#'
#' @param item_parameters A [tibble::tibble()] or with the item parameters of
#'  the DCM model as returned by `tidy()` method.
#' @param qmatrix Q-matrix of the test. If NULL, it will be recovered from the
#'  item_parameters argument. The Q-matrix is only needed to know the number
#'  of attributes and their names.
#' @param profiles Which profiles to consider for the calculations
#'
#' @returns a tibble with the attribute columns, response column, item and
#'  probability
#' @export
#'
get_response_probabilities <- function(
    item_parameters,
    qmatrix = NULL,
    profiles = NULL
) {

  # TODO: qmatrix and profiles should be exclusive

  if (is.null(qmatrix)) {
    qmatrix <- get_qmatrix(item_parameters)
  }

  if (is.null(profiles)) {
    profiles <- create_patterns(
      ncol(qmatrix) - 1,
      column_labels = colnames(qmatrix)[-1]
    )
  }

  # Probabilities by profile - This is the correct ones
  probability_matrix <- generate_responses(
    item_parameters,
    profiles,
    get_probs = TRUE
  )

  rownames(probability_matrix) <- NULL

  # This assumes only dichotomous items.
  response_probability <- rbind(profiles, profiles) |>
    as_tibble() |>
    mutate(
      response = rep(
        c(1,0),
        each = nrow(probability_matrix)
      )
    ) |>
    cbind(
      rbind(
        probability_matrix,
        1 - probability_matrix
      )
    ) |>
    tidyr::pivot_longer(
      cols = all_of(colnames(probability_matrix)),
      names_to = "item",
      values_to = "probability"
    )

  # return
  response_probability
}
