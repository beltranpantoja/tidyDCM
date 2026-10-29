#' Generate responses to a test
#'
#' Generate a set of expected responses by calculating the probability of a
#'  correct response for each examinee. The function uses the `link_fun` column
#'  of the item_parameters tibble to calculate the probabilities of the correct
#'  link.
#'
#' @param item_parameters A [tibble::tibble()] or with the item parameters of
#'  the DCM model as returned by `tidy()` method.
#' @param examinees A tibble-like or binary matrix of respondents
#' @param get_probs Should return the probabilities of a correct response or the
#'  realized binary. Default is `FALSE`.
#' @param link_fun,rule A single string or a vector the same length as the items
#'  to be used to convert the item parameters to probabilities.
#'
#' @returns a matrix of binary responses or a double representing the
#'  probability of a correct response.
#' @export
#'
generate_responses <- function(
    item_parameters,
    examinees,
    get_probs = FALSE,
    link_fun = NULL,
    rule = NULL
) {

  # Standardize examinees argument
  examinees <- as_examinees(examinees)

  # TODO: Add result check: when passing all possible profiles, is the
  #   probability bounded?

  # TODO: I should be able to pass a Q-matrix that then gets passed to
  # the other methods.

  # TODO: Allow the user to pass a link_fun for when is not in the tibble
  # TODO: Allow different link functions to be present. I can use group_by or
  # something like that.


  # There should be a single link function. Only implemented for logit and
  # identity
  # TODO: Implement for log
  # TODO: Handle errors
  if (is.null(link_fun)) {
    link_fun <- item_parameters$link_fun |>
      unique() |>
      match.arg(choices = c("logit", "identity"))
  } else {
    link_fun <- link_fun |> match.arg(choices = c("logit", "identity"))
  }

  # TODO: does this work?
  if (!is.null(rule)) {
    item_parameters$rule <- rule
  }




  # ========================================================================
  # EXAMINEES EFFECTS MASK
  # Convert the examinees mastery to a mask for the matrix of effects
  # ========================================================================

  # Formula of all attrs with interactions
  # e.g.  ~ (A1 + A2 + A3)^3
  formula_str <- paste0(
    "~ (",
    paste(names(examinees),
          collapse = " + "),
    ")^",
    ncol(examinees)
  )

  # model.matrix automatically builds the intercept and all interaction effects
  examinee_effects <- stats::model.matrix(
    stats::as.formula(formula_str),
    data = examinees) |>
    as.matrix()

  # ========================================================================
  # ITEM EFFECTS MATRIX
  # Convert the items parameters to a matrix of effects and interactions by item
  # ========================================================================

  # We get the saturated version of the model
  item_effects <-
    item_parameters |>
    to_saturated_items() |>
    parameters_to_table(TRUE) |>
    tibble::column_to_rownames(var = "item") |>
    as.matrix()


  # This product gives us the logit matrix respondents by items
  values_matrix <- examinee_effects %*% t(item_effects)

  response_probs <- switch(
    link_fun,
    identity = values_matrix,
    logit = exp(values_matrix) / (1 + exp(values_matrix))
  )

  # If we only want the probs then return early
  if (get_probs) {
    return(response_probs)
  }

  # Else, we create a random matrix to generate the responses
  mask <- stats::runif(length(response_probs))

  ((response_probs > mask) * 1)

}
