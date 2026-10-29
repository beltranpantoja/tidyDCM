#' Argument type: DCM-object
#'
#' @description
#' This page describes the `<DCM-object>` argument modifier which
#' indicates that the argument should be a DCM model object fitted from one of
#' the implemented function.
#'
#' # What's a DCM object?
#'
#' As DCMs become more popular and more research is done about them, more models
#' and tools for working with them are developed. This leads to different
#' implementations and object structures of models that are mathematically the
#' same creating a Tower of Babel situation. The `tidyDCM` library provides a
#' unified syntax to work with all these models.
#'
#' # How to use
#'
#' Currently we only support the [GDINA::GDINA] and [CDM::gdina] functions. With
#' some limitations (e.g. only support one group).
#'
#' @keywords internal
#' @name args_DCM_object
NULL



#' Item parameters tibble
#'
#' @description
#' This page describes the `<item-parameters>` tibble. Functions like
#' [get_items()] return a tibble that contains all the information related to
#' the test extracted from a <[`DCM-object`][args_DCM_object]>. These tibbles
#' can also be used in other functions like [generate_responses()] or
#' [get_qmatrix()].
#'
#' # Columns
#'
#'  One of the main objectives of `tidyDCM` is to abstract the complexity of
#'  working with several different libraries. Thus, it always returns the same
#'  columns:
#'
#'  * __number__ Integer. Number of the item used as identifier.
#'  * __item__ Character. Name of the item used as identifier extracted from the
#'    Q-matrix.
#'  * __rule__ Character. Condensation rule used to convert the item parameters
#'    to correct response probabilities (e.g. GDINA, DINA, RDINA). See "Rules,
#'    link functions and DCM models" section for more information.
#'  * __link_fun__ Character. Link function (e.g. logit, identity)
#'  * __term__ Character. The attributes required for this parameter to be
#'    considered (e.g. the term "1-3" is included for masters of both attribute
#'    1 and 3). See "Terms and Logic Gates" section.
#'  * __order__ Integer. How many attributes are included in the term. Intercepts
#'    are 0, main effects 1, two-way effects 2, and so on.
#'  * __estimate__ Double. The parameter itself.
#'  * __SE__ Double. If it was estimated, this is also included.
#'
#'
#' # Rules, link functions and DCM models
#'
#' DCMs come in all shapes and sizes with different condensation rules and link
#' functions having different names which makes sometimes things be confusing.
#' For example the log-linear cognitive diagnosis model (LCDM; Henson,
#' Templin & Willse, 2009) and the generalized deterministic inputs noisy “and”
#' gate model (G-DINA; de la Torre, 2011) are both saturated models, but the
#' former uses a logit link while the latter uses the identity link. Another
#' similar example is the DINA model. In it's original description, the DINA
#' model was described as having a two parameters per item: slip and guess.
#' However, in current implementations, is more common to actually use a
#' reparametrized version of the DINA model (RDINA; DeCarlo, 2011), most of the
#' time this ignored and just the original name DINA is used, but this can lead
#' to confusions.
#'
#' # How do we solve this?
#'
#' We want to prioritize clarity and simplicity while also being coherent we
#' what's already out there. What this means is that, when possible, we try
#' to keep things consistent with the names the packages already use with two
#' exceptions:
#'
#' 1. __All saturated models are named "GDINA"__. The main reasons for this is
#'  that the 2011 article by de la Torre shows in detail all the generalized and
#'  a few restricted models with different link functions. Meaning, the term
#'  "GDINA" does not really imply the identity link function, whereas other
#'  general models such as the LCDM do imply one and we want to separate the role
#'  of the link function from the condensation rule.
#'
#' 2. __Proper use of the DINA/O__. Some functions like [GDINA::GDINA] and
#'  [CDM::gdina] are made to fit general models or restricted versions of it,
#'  meaning that when passing "DINA" as an argument what we get back is not
#'  actually the "DINA" model in its canonical form (slip and guess parameters),
#'  but a reparametrized version. In that case we actually change the name
#'  of the rule to reflect the actual model.
#'
#'  ```
#'  mod_1 <- CDM::gdina(responses, qmatrix, rule="DINO")
#'  mod_2 <- GDINA::GDINA(responses, qmatrix, model="DINA")
#'
#'  # The rule column is "RDINO" and "RDINA" respectively.
#'  mod_1 |> get_items()
#'  mod_2 |> get_items()
#'  ```
#'
#' # Terms and Logic Gates
#'
#' TODO
#'
#' @references
#' de la Torre, J. (2011). The Generalized DINA Model Framework. Psychometrika, 76(2), 179–199. https://doi.org/10.1007/s11336-011-9207-7
#' DeCarlo L. T. (2011). On the analysis of fraction subtraction data: The DINA model, classification, latent class sizes, and the Q-matrix. Applied Psychological Measurement, 35(1), 8–26. https://doi.org/10.1177/0146621610377081
#' Henson, R. A., Templin, J. L., & Willse, J. T. (2009). Defining a Family of Cognitive Diagnosis Models Using Log-Linear Models with Latent Variables. Psychometrika, 74(2), 191–210. https://doi.org/10.1007/s11336-008-9089-5
#'
#' @keywords internal
#' @name args_item_parameters
NULL
