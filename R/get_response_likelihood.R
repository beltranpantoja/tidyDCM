#' Generate response likelihood
#'
#' This is an extension of [`get_response_probabilities`] meant to simplify
#' some common calculations. While [`get_response_probabilities`] returns
#' the probabilities of the response to a single item (i.e. \eqn{P(X=h_i | \alpha_l)})
#' This function calculates the probability of the complete response pattern, meaning:
#'
#' \deqn{
#'    P(X = x | \alpha_l) = \prod_J P(X = x_i | \alpha_l)
#'    }
#'
#' To avoid having the result of this function explode in unexpected ways, the
#' arguments are responses and profiles are considered paired not crossed (the
#' first response it's assumed to come from the first profile and so on). Meaning
#' the size of the resulting tibble matches the number of profiles and responses.
#'
#'
#' @param item_parameters A [tibble::tibble()] or with the item parameters of
#'  the DCM model as returned by `tidy()` method.
#' @param profiles Which profiles to consider for the calculations. Must have the
#'  same number of rows as the `response` argument.
#' @param responses The complete response pattern. Must have the same number of rows
#'  as the `profiles` argument.
#'
#' @returns a tibble with the profile, the responses and its likelihood.
#' @export
#'
get_response_likelihood <- function(
    item_parameters,
    profiles,
    responses
) {

  complete_profiles <- create_patterns(
    ncol(profiles),
    column_labels = colnames(profiles)
  )

  response_probability <- get_response_probabilities(
    item_parameters,
    profiles = complete_profiles # We pass the profiles to get the correct labels
  )

  base_df <- cbind(
    profiles,
    responses
  ) |>
    mutate(
      id = dplyr::row_number()
    )

  probability_vector <- base_df |>
    tidyr::pivot_longer(
      cols = all_of(colnames(responses)),
      names_to = "item",
      values_to = "response"
    ) |>
    left_join(
      response_probability,
      by = c(colnames(profiles), "item", "response")
    ) |>
    dplyr::summarise(
      probability = prod(.data$probability),
      .by = "id"
    ) |>
    dplyr::pull(.data$probability)



  base_df |>
    mutate(
      probability = probability_vector
    ) |>
    select(-"id")

}
