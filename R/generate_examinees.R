#' Generate Examinees
#'
#' Generates a binary matrix by dichotomizing independent standard normal
#' draws. This is just a utility function as `tidyDCM` is not meant to be a
#' package heavy on random generation. See the vignettes to read more about
#' what alternatives are there for random generation.
#'
#' @param num_examinees Number of examinees to generate.
#' @param num_attr Number of attributes.
#' @param base_rate Proportion of 1s in each column. Either a single value
#'   (recycled across attributes) or a vector of length `num_attr`.
#' @param attr_prefix A single string used to build attribute names as
#'   `<attr_prefix>1`, `<attr_prefix>2`, and so on. Defaults to `"attr"` when
#'   both `attr_prefix` and `attr_labels` are `NULL`.
#' @param attr_labels A character vector of length `num_attr` with the exact
#'   attribute names to use. Cannot be combined with `attr_prefix`.
#'
#' @returns A [tibble::tibble()] with `num_examinees` rows. The first column,
#'   `id`, is an integer examinee identifier; the remaining `num_attr` columns
#'   are integer (0/1) attribute indicators.
#' @export
#'
#' @examples
#' generate_examinees(5, 3, base_rate = 0.5)
#' generate_examinees(5, 3, base_rate = c(0.3, 0.5, 0.7), attr_prefix = "A")
#' generate_examinees(5, 3, base_rate = 0.5, attr_labels = c("add", "sub", "mul"))
generate_examinees <- function(
    num_examinees,
    num_attr,
    base_rate,
    attr_prefix = NULL,
    attr_labels = NULL
) {

  # Validation and cleaning
  vctrs::vec_assert(num_examinees, size = 1L)
  vctrs::vec_assert(num_attr, size = 1L)
  num_examinees <- vctrs::vec_cast(num_examinees, integer())
  num_attr <- vctrs::vec_cast(num_attr, integer())

  if (is.na(num_examinees) || num_examinees < 1L) {
    rlang::abort(c("`num_examinees` must be a positive whole number.",
                   "x" = sprintf("Supplied: %s.", format(num_examinees))))
  }
  if (is.na(num_attr) || num_attr < 1L) {
    rlang::abort(c("`num_attr` must be a positive whole number.",
                   "x" = sprintf("Supplied: %s.", format(num_attr))))
  }

  base_rate <- vctrs::vec_recycle(vctrs::vec_cast(base_rate, double()), num_attr)
  if (anyNA(base_rate) || any(base_rate <= 0 | base_rate >= 1)) {
    rlang::abort(c("All values of `base_rate` must be strictly between 0 and 1.",
                   "x" = "`base_rate` contains NA or values outside (0, 1)."))
  }

  # Attribute names
  if (!is.null(attr_prefix) && !is.null(attr_labels)) {
    rlang::abort(c("Supply only one of `attr_prefix` and `attr_labels`.",
                   "x" = "Both were provided."))
  }

  if (!is.null(attr_labels)) {
    if (!is.character(attr_labels)) {
      rlang::abort("`attr_labels` must be a character vector.")
    }
    if (length(attr_labels) != num_attr) {
      rlang::abort(c("`attr_labels` must have length `num_attr`.",
                     "x" = sprintf("Expected %d labels, got %d.",
                                   num_attr, length(attr_labels))))
    }
    if (anyNA(attr_labels) || any(attr_labels == "")) {
      rlang::abort("`attr_labels` must not contain NA or empty strings.")
    }
    if (anyDuplicated(attr_labels) > 0L) {
      rlang::abort("`attr_labels` must be unique.")
    }
    if ("id" %in% attr_labels) {
      rlang::abort(c("`attr_labels` cannot contain \"id\".",
                     "i" = "`id` is reserved for the examinee identifier."))
    }
    labels <- attr_labels
  } else {
    if (is.null(attr_prefix)) {
      attr_prefix <- "attr"
    }
    if (!is.character(attr_prefix) || length(attr_prefix) != 1L ||
        is.na(attr_prefix) || attr_prefix == "") {
      rlang::abort("`attr_prefix` must be a single non-empty string.")
    }
    labels <- paste0(attr_prefix, seq_len(num_attr))
    if ("id" %in% labels) {
      rlang::abort(c("Generated attribute names cannot contain \"id\".",
                     "i" = "`id` is reserved for the examinee identifier."))
    }
  }

  # Generate normal matrix
  Z <- matrix(
    stats::rnorm(num_examinees * num_attr),
    nrow = num_examinees,
    ncol = num_attr
  )

  # Transform to binary
  thr <- stats::qnorm(1 - base_rate)  # P(Z > thr) = p
  X <- sweep(Z, 2, thr, FUN = ">") * 1L
  storage.mode(X) <- "integer"
  colnames(X) <- labels

  tibble::as_tibble(X)

}
