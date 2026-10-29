#' Create an items tibble from a Qmatrix and a set of rules
#'
#' This is an utility function to create item parameter tibble based on a
#' Q-matrix and a set of rules.
#'
#' @param qmatrix A Q-matrix. Either as a tibble or as a matrix.
#' @param rule A rule for the DCM model. If it's NULL the saturated model
#'  (GDINA) is assumed. If a single character is used, then all the items are
#'  assumed to use the same rule.
#' @param link_fun A link function for the DCM model. If it's NULL the identity
#'  link is assumed. If a single character is used, then all the items are
#'  assumed to have the same link.
#'
#' @returns a [tibble::tibble()]
#' @keywords internal
items_from_qmatrix <- function(
    qmatrix,
    rule = NULL,
    link_fun = NULL
    ) {

  # Standardize qmatrix
  qmatrix <- as_qmatrix(qmatrix)


  # Default values if null
  rule <- rule %||% "GDINA"
  link_fun <- link_fun %||% "identity"

  # Recycle to the complete size
  rule <- vctrs::vec_recycle(rule, nrow(qmatrix), x_arg="rule")
  link_fun <- vctrs::vec_recycle(link_fun, nrow(qmatrix), x_arg="link_fun")

  attr_cols <- colnames(qmatrix)[-1]

  # Get all possible combinations of q-vectors
  all_combinations <-
    create_patterns(length(attr_cols)) |>
    as_tibble() |>
    mutate(across(everything(), as.integer)) |>
    setNames(attr_cols)



  # We write the conditions using rlang for simplicity
  # This creates the condition that the qvec columns have to be equal
  # or greater than the observed complete combinations
  conditions <- purrr::map(
    attr_cols,
    ~ rlang::expr(
      !!rlang::sym(paste0(.x, "_qvec")) >= !!rlang::sym(.x)
    )
  )


  # We cross join them to create all combinations and the use
  # the conditions to remove all the rows with more 1s than allowed.
  saturated_base_items <- qmatrix |>
    stats::setNames(c("item", paste0(attr_cols, "_qvec"))) |>
    mutate(
      number=dplyr::row_number(),
      rule = rule,
      link_fun = link_fun,
      ) |>
    dplyr::cross_join(all_combinations) |>
    filter(!!!conditions) |>
    select("number", "item", "rule", "link_fun", all_of(attr_cols))

  # Now we create the item tibble with cols: item term order
  saturated_items <- saturated_base_items |>
    mutate(
      order = rowSums(pick(all_of(attr_cols))),
      across(
        all_of(attr_cols),
        \(col) {col * match(dplyr::cur_column(), attr_cols)}
      ),
      across(
        all_of(attr_cols),
        \(col) if_else(col == 0, NA, col)
      )
    ) |>
    tidyr::unite(
      "term",
      all_of(attr_cols),
      sep="-",
      na.rm=TRUE
    ) |>
    mutate(
      term = if_else(.data$term == "", "0", .data$term)
    )


  # This gives us the logic to filter the parameters by the rule
  # of each item
  item_condition <- function(rule) {
    if (rule == "GDINA") {
      rlang::expr(TRUE)
    } else if (grepl("^GDINA\\d+$", rule)) {
      n <- as.integer(sub("GDINA", "", rule))
      rlang::expr(order <= !!n)
    } else if (rule == "ACDM") {
      rlang::expr(order %in% c(0, 1))
    } else if (grepl("^R?DIN[A|O]$", rule)) {
      rlang::expr(order == 0 | order == max(order))
    } else {
      stop(paste0("Unknown rule: ", rule))
    }
  }

  # We iterate over each item to get only the valid terms
  saturated_items |>
    dplyr::mutate(
      # We convert the terms of DINO to mean "or"
      term = if_else(
        rule %in% c("DINO", "RDINO"),
        chartr("-", "|", .data$term),
        .data$term
      )
    ) |>
    dplyr::group_by(.data$item) |>
    dplyr::group_modify(
      \(item_df, ...) {
        item_rule <- unique(item_df$rule)
        condition <- item_condition(item_rule)

        # We filter the item rows by the specific condition
        item_df |>
          filter(!!condition)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$number) |>
    mutate(
    # We change the "order" term for the DINA/O and the RDINO models
    # For the RDINA we keep the saturated order.
    order = if_else(
      stringr::str_detect(.data$rule, "^(?:DI[AO]|RDINO)$") & .data$order != 0,
      1,
      .data$order
      )
    )

}
