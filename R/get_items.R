#' @importFrom generics tidy
#' @export
generics::tidy


#' Get item parameters from a fitted DCM
#'
#' Returns the item parameters of a fitted diagnostic classification
#' model. This is an alias of [generics::tidy], so any method registered
#' for `tidy()` is used.
#'
#' @name get_items
#' @usage get_items(x, ...)
#'
#' @param x A <[`DCM-object`][args_DCM_object]>.
#' @inheritParams rlang::args_dots_used
#'
#' @returns A tibble of type <[`item-parameters`][args_item_parameters]>
#' @export
#' @examples
#' # Loading data
#'
#' data(sim10GDINA, package = "GDINA")
#' responses <- sim10GDINA$simdat
#' qmatrix <- sim10GDINA$simQ
#'
#' # Fitting models
#' mod_1 <- CDM::gdina(responses, qmatrix, link="logit")
#' mod_2 <- GDINA::GDINA(responses, qmatrix, model="ACDM")
#' mod_3 <- CDM::gdina(responses, qmatrix, rule="DINA")
#'
#' # Getting the item parameters
#' mod_1 |> get_items()
#' mod_2 |> get_items()
#' mod_3 |> get_items()
get_items <- function(
    x,
    ...
) {
  generics::tidy(x, ...)
}


#' @export
tidy.gdina <- function(x, ...) {


  estimates_df <- x |> coef()
  attr_cols <- x |> get_qmatrix() |> select(-"item") |> colnames()

  column_order <- c(
    "number",
    "item",
    "rule",
    "link_fun",
    "term",
    "order",
    "estimate",
    "SE"
  )

  estimates_df |>
    as_tibble() |>
    mutate(
      attributes = stringr::str_split(.data$partype.attr, "-"),
      term = purrr::map_chr(
        .data$attributes,
        ~ paste(match(.x, attr_cols, nomatch="0"), collapse = "-")
      ),
      term = if_else(
        .data$rule == "DINO",
        chartr("-", "|", .data$term),
        .data$term
      ),
      order = lengths(.data$attributes),
      order = if_else(.data$term == "0", 0, .data$order),
      # TODO: this solution is a little hacky, need to streamline it
      order = if_else(
        .data$rule == "DINO" & order != 0,
        1,
        .data$order
      ),
      rule = stringr::str_replace(.data$rule, "^DIN", "RDIN")
    ) |>
    rename(
      number = "itemno",
      estimate = "est",
      SE = "se",
      link_fun = "link"
    ) |>
    select(any_of(column_order))


}



#' @export
tidy.GDINA <- function(x, ...) {

  # Get basic elements from the GDINA object
  dat <- get_responses(x)
  coef_mat <- stats::coef(x, what = "delta", withSE = TRUE, digits=999)
  item_labels <- colnames(dat)
  qmatrix <- x |> get_qmatrix()
  link_fun <- GDINA::extract(x, what="linkfunc")
  rule <- stringr::str_remove(x$model, "LOG(IT)?") |>
    stringr::str_replace("^DIN", "RDIN")


  # Correctly label the coefficients matrix
  names(coef_mat) <- item_labels


  # Construct the parameters tibble
  base_parameters_tibble <- items_from_qmatrix(qmatrix, rule, link_fun)

  # Convert the coefficients to the estimates tibbles
  # The order is the same
  estimates_df <- coef_mat |>
    purrr::map(
      \(item_coef_df) {
        item_coef_df |>
          as_tibble() |>
          # TODO: This might fail if there aren't SE for some reason
          mutate(what = c("estimate", "SE")) |>
          tidyr::pivot_longer(
            cols=c(everything(), -"what"),
            values_to = "value",
            names_to = "parameter"
          ) |>
          tidyr::pivot_wider(
            names_from = "what",
            values_from = "value"
          )
      }
    ) |>
    bind_rows(.id = "item") |>
    select("estimate", "SE")


  column_order <- c(
    "number",
    "item",
    "rule",
    "link_fun",
    "term",
    "order",
    "estimate",
    "SE"
  )

  # Finally, join them all
  base_parameters_tibble |>
    dplyr::bind_cols(estimates_df) |>
    select(any_of(column_order))


}

