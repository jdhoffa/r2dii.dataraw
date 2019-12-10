#' Datasets to bridge (translate) common sector-classification codes
#'
#' These datasets help to translate (bridge) common sector classification codes
#' from the wild to codes we use in 2dii such as 'power', 'oil and gas', 'coal',
#' 'automotive', 'aviation', 'concrete', 'steel', and 'shipping'.
#'
#' @aliases isic_classification nace_classification naics_classification
#' @seealso [data_dictionary()].
#'
#' @family datasets for bridging sector classification codes
#'
#' @return A [dplyr::tibble].
#'
#' @export
#'
#' @examples
#' isic_classification
#' nace_classification
#' naics_classification
"isic_classification"

#' @export
#' @rdname isic_classification
"nace_classification"

#' @export
#' @rdname isic_classification
"naics_classification"
