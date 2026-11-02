#' Get the posterior probability of a fitted model
#'
#' @param object A <[`DCM-object`][args_DCM_object]>.
#' @inheritParams rlang::args_dots_used
#'
#' @returns A tibble with all profiles and their posterior probability.
#' @export
get_posterior <- function(
    object,
    ...
) {
  UseMethod("get_posterior")
}



#' @export
get_posterior.GDINA <- function(
    object,
    ...
) {

  attribute_labels <- get_qmatrix(object) |>
    select(-"item") |>
    colnames()

  object$posterior.prob[1, ] |>
    as_tibble(rownames = "profile") |>
    rename(
      posterior = "value"
    ) |>
    tidyr::separate_wider_position(
      cols = "profile",
      widths = setNames(
        rep(1, length(attribute_labels)),
        attribute_labels
      )
    ) |>
    mutate(
      across(
        all_of(attribute_labels),
        as.integer
      )
    )
}

#' @export
get_posterior.gdina <- function(
    object,
    ...
) {

  attribute_labels <- get_qmatrix(object) |>
    select(-"item") |>
    colnames()

  object$attribute.patt |>
    as_tibble(rownames = "profile") |>
    select("profile", "class.prob") |>
    rename(
      posterior = "class.prob"
    ) |>
    tidyr::separate_wider_position(
      cols = "profile",
      widths = setNames(
        rep(1, length(attribute_labels)),
        attribute_labels
      )
    ) |>
    mutate(
      across(
        all_of(attribute_labels),
        as.integer
      )
    )

}





