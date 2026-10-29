#' Convert an item parameter tibble into it's equivalent saturated version
#'
#'
#' @param item_parameters Tibble or data frame of item parameters.
#' @param qmatrix Not used yet.
#' @returns A tibble with the saturated equivalent parameters
#' @keywords internal
to_saturated_items <- function(
    item_parameters,
    qmatrix = NULL
    ) {

  if (is.null(qmatrix)) {
    qmatrix <- item_parameters |>
      get_qmatrix()
  }

  # Transform DINA/O to RDINA/O
  # Fix DINO

  # Fix either item/rule
  item_rules <- item_parameters |>
    dplyr::distinct(.data$item, .data$rule)

  qmatrix |>
    items_from_qmatrix() |>
    select("number", "item", "rule", "term", "order") |>
    left_join(
      item_parameters |>
        select("number", "item", "term", "estimate") |>
        # This converts DINO into main effects
        tidyr::separate_longer_delim(
          .data$term, "|"
        ),
      by = dplyr::join_by("number", "item", "term")
    ) |>
    mutate(
      estimate = tidyr::replace_na(.data$estimate, 0)
    ) |>
    saturate_rdino(
      item_rules |> filter(.data$rule=="RDINO") |> dplyr::pull(.data$item)
    )


}




#' Convert a DINA/O tibble of item parameters to it's RDINA/O version
#'
#'
#' @param item_parameters Tibble or data frame of item parameters.
#' @param qmatrix Not used yet.
#' @returns A tibble with the saturated equivalent parameters
#' @keywords internal
to_RDIN <- function(
    item_parameters,
    qmatrix = NULL
) {

  # TODO: DINA shouldn't have link_fun, as is defined as the identity.

  item_parameters |>
    mutate(
      order = if_else(.data$term != 0, 1, 0)
    ) |>
    mutate(
      estimate = if_else(
        .data$order == 1,
        # main effect: 1 - s - g
        1 - .data$estimate - .data$estimate[.data$order == 0],
        .data$estimate
      ),
      rule = paste0("R", .data$rule),
      .by = dplyr::all_of(c("number", "item", "link_fun"))
    )
}

#' Utility to convert RDINO parameters to saturated version
#'
#'
#' @param item_parameters Tibble or data frame of item parameters.
#' @param items List of item names.
#' @returns A tibble with the saturated equivalent parameters of the RDINO
#' @keywords internal
saturate_rdino <- function(item_parameters, items) {
  item_parameters |>
    mutate(
      estimate = dplyr::case_when(
        # other items no change
        !.data$item %in% items ~ .data$estimate,
        # intercept, no change
        .data$order == 0       ~ .data$estimate,
        # even terms are equal to negative the main effect, odds are positive.
        .data$order %% 2 == 0  ~ -1*dplyr::first(.data$estimate[.data$order == 1]),
        .default         = dplyr::first(.data$estimate[.data$order == 1])
      ),
      .by = "item"
    )
}
