#' Data dictionary
#'
#' * `data_dictionary` is a dataset that defines the columns of other r2dii
#' datasets.
#' * `create_data_dictionary()` is a function that helps extend
#' `data_dictionary`. It is particularly useful when combined with
#' `datapasta::tibble_paste()`. See details.
#'
#' You may combine `create_data_dictionary()` with `datapasta::tribble_paste()`
#' to create a tibble you can conveniently fill. For example, Code like this:
#'
#' ```R
#' create_data_dictionary("loanbook_demo", package = "r2dii.dataraw") %>%
#'   datapasta::tribble_paste()
#' ```
#'
#' outputs something like this:
#'
#' ```R
#' tibble::tribble(
#'   ~dataset,                                         ~column, ~definition,
#'   "loanbook_demo",                                "id_loan",          NA,
#'   "loanbook_demo",                    "id_direct_loantaker",          NA,
#' ...
#' )
#' ```
#'
#' You can then replace each `NA` with a corresponding `definition`.
#'
#' @param dataset A length-1 character string giving the name of a dataset.
#' @param package A length-1 character string giving the name of a package.
#'
#' @family demo datasets
#'
#' @return A [tibble::tibble].
#'
#' @keywords internal
#'
#' @examples
#' data_dictionary
#'
#' # This is particulary useful when combined with `datapasta::tribble_paste()`
#' create_data_dictionary("loanbook_demo", package = "r2dii.dataraw")
"data_dictionary"

#' @rdname data_dictionary
#' @export
create_data_dictionary <- function(dataset, package = NULL) {
  package <- package %||% fs::path_file(usethis::proj_get())

  dataset_has_length_1 <- identical(length(dataset), 1L)
  stopifnot(
    dataset_has_length_1,
    is.character(dataset),
    is.character(package)
  )

  dataset_ <- get(dataset, envir = as.environment(glue("package:{package}")))
  tibble::tibble(
    dataset = dataset,
    column = names(dataset_),
    definition = NA_character_
  )
}
