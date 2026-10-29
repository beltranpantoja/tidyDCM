#' Extract the Q-matrix of a DCM object
#'
#' @param object Either a fitted model of type [CDM::gdina]/[GDINA::GDINA]
#'  or a tibble/data.frame as returned from applying the `tidy` method to a DCM
#'  model.
#' @param check_rule Checks that the number of parameters in the tibble
#'  coincides with the number of expected parameters for a given condensation
#'  rule. Ignored if the passed object is a DCM model. Currently accepts:
#'  "GDINA", "ACDM", "DINA", "DINO". If you do not wish to check the number
#'  of parameters you can pass NULL.
#' @inheritParams rlang::args_dots_used
#'
#' @returns the Q-matrix in a tibble.
#' @export
get_qmatrix <- function(
    object,
    check_rule = NULL,
    ...
    ) {
  UseMethod("get_qmatrix")
}



#' @export
get_qmatrix.GDINA <- function(
    object,
    ...
    ) {
  data.frame(
    item = colnames(get_responses(object)),
    object$options$Q
    ) |>
    as_tibble()
}

#' @export
get_qmatrix.gdina <- function(
    object,
    ...
    ) {
  object$q.matrix |>
    as_tibble(
      rownames = "item"
    )
  }

#' @export
get_qmatrix.data.frame <- function(
    object,
    check_rule = c(
      "GDINA",
      "ACDM",
      "DINA",
      "DINO"
    ),
    ...
    ) {

  if (!is.null(check_rule)) {
    check_rule <- rlang::arg_match0(
      arg = check_rule,
      values = c("GDINA", "ACDM", "DINA", "DINO")
    )
  }


  # TODO: add the possibility of passing attribute labels
  qmatrix <- object |>
    select("item", "term") |>
    dplyr::bind_cols(
      qvector_from_terms(object$term)
    ) |>
    dplyr::summarise(
      across(
        where(is.integer),
        \(col) max(col)
      ),
      .by="item"
    )

  # Skipping check for now
  if (FALSE) {
    # !is.null(check_rule)
    .check_number_of_parameters(
      object,
      check_rule,
      call = rlang::caller_env()
      )
  }

  qmatrix

}


# =========================================================================
# Utility function to validate the number of parameters in the item coef
# =========================================================================

#' @keywords internal
.check_number_of_parameters <- function(
    object,
    check_rule,
    call = rlang::caller_env()
    ) {

  # TODO: need to implement the check for rules with restricted interactions
  # like GDINA2 GDINA3 etc..

  num_params <- object |>
    get_qmatrix(check_rule = NULL) |>
    mutate(
      total_att = rowSums(pick(where(is.integer))),
      DIN = 2,
      main_effects = .data$total_att + 1,
      saturated = 2^.data$total_att
    ) |>
    select("item", "DIN":"saturated")

  expected_params <- switch(
    check_rule,
    GDINA = num_params$saturated,
    GDINA1 = num_params$main_effects,
    ACDM = num_params$main_effects,
    DINA = num_params$DIN,
    DINO = num_params$DIN
  )

  non_conforming <- object |>
    dplyr::count(.data$item, name="observed") |>
    mutate(
      expected = expected_params
    ) |>
    filter(
      .data$observed != .data$expected
    )

  if (nrow(non_conforming) > 0) {
    # Format each row into a descriptive bullet point
    items_summary <- sprintf(
      "%s: observed %d, expected %g",
      non_conforming$item,
      non_conforming$observed,
      non_conforming$expected
    )

    cli::cli_abort(
      c(
        "x" = "Non-conforming item parameters detected:",
        "*" = items_summary,
        "i" = "Please check input data for missing or unexpected parameters.",
        "i" = "Pass {.code check_rule = NULL} if you want to ignore this check."
        ),
      call = call
    )
  }

}


#' Utility function to get Q-vectors from the term column.
#'
#' This is an utility function to expand a column of terms to the respective
#' Q-vector. For example the term "1-3" becomes 1 0 1.
#'
#' @param terms List of terms
#' @param attr_labels Attribute labels to use when returning q-vectors. If NULL, #'  names are created by default.
#' @param attr_label_prefix Prefix for attribute columns if not provided.
#'  Default is "A".
#'
#' @returns a [tibble::tibble()]
#' @noRd
#' @keywords internal
qvector_from_terms <- function(
    terms,
    attr_labels = NULL,
    attr_label_prefix = "A"
    ) {

  # TODO: implement for named terms like "attr1-attr2"
  term_vectors <-
    # Split by either "-" or "|"
    stringr::str_split(terms, "-|\\|") |>
    purrr::map(as.integer)

  num_attrs <- term_vectors |> unlist() |> max()

  # Check if null
  if (is.null(attr_labels)) {
    attr_labels <- paste0(attr_label_prefix, seq_len(num_attrs))
  }

  purrr::map(
    seq_len(num_attrs),
    \(k) purrr::map_int(
      term_vectors,
      \(p) (k %in% p)
      )
    ) |>
    rlang::set_names(attr_labels) |>
    as_tibble()


}
