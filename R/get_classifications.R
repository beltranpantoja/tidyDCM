#' @importFrom generics augment
#' @export
generics::augment

#' Get classifications from a fitted DCM
#'
#' Returns the respondent classifications of a fitted diagnostic classification
#' model. This is a convenience wrapper around [generics::augment], so any
#' method registered for `augment()` is used.
#'
#'
#' @param x A fitted model from [GDINA::GDINA] or [CDM::gdina].
#' @param method The classification type: `"EAP"` (default), `"MAP"`,
#'   `"MLE"`, or `"marginal"`.
#' @param ... Further arguments passed to the `augment()` method.
#'
#' @returns A tibble with the requested classifications.
#' @export
get_classifications <- function(
    x,
    method = c("EAP", "MAP", "MLE", "marginal"),
    ...
) {
  method <- rlang::arg_match(method)
  generics::augment(x, method = method, ...)
}

#' @export
augment.GDINA <- function(
    x,
    method =  c("EAP", "MAP", "MLE", "marginal"),
    ...
    ) {

  if (missing(method)) {
    method <- match.arg(method)

    cli::cli_inform(c(
      "i" = "No `method` specified. Defaulting to {.val {method}}."
    ))
  } else {
    method <- match.arg(method)
  }

  class_method <- if (method=="marginal") "mp" else method

  attr_labels <- x |>
    get_qmatrix() |>
    select(-"item") |>
    colnames()


  GDINA::personparm(x, class_method) |>
    as_tibble() |>
    select(-any_of("multimodes")) |>
    setNames(attr_labels)

}

#' @export
augment.gdina <- function(
    x,
    method =  c("EAP", "MAP", "MLE", "marginal"),
    ...
    ) {

  if (missing(method)) {
    method <- match.arg(method)

    cli::cli_inform(c(
      "i" = "No `method` specified. Defaulting to {.val {method}}."
    ))
  } else {
    method <- match.arg(method)
  }

  attr_labels <- x |>
    get_qmatrix() |>
    select(-"item") |>
    colnames()

  # Named width vector for splitting MAP and MLE
  widths_vec <- setNames(rep(1, length(attr_labels)), attr_labels)

  # Baseline is the marginal
  classification_df <- x$pattern |>
    as_tibble() |>
    select(starts_with("post."))

  if (method == "EAP") {
    classification_df <- classification_df |>
      mutate(
        across(
          everything(),
          \(val) (val >= .5)*1
          )
      )
  } else if (method == "MAP") {
    classification_df <- x$pattern$map.est |>
      as_tibble() |>
      tidyr::separate_wider_position(
        everything(),
        widths=widths_vec
      )
  } else if (method == "MLE") {
    classification_df <- x$pattern$mle.est |>
      as_tibble() |>
      tidyr::separate_wider_position(
        everything(),
        widths=widths_vec
      )
  }

  classification_df |>
    setNames(attr_labels)

}

