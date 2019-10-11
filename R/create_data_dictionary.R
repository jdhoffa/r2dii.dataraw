#' Help extend the `data_dictionary`
#'
#' This function takes the exported dataset from a package and creates a
#' tibble suitable for extending the `data_dictionary`.
#'
#' You may combine `create_data_dictionary()` with `datapasta::tribble_paste()` to
#' create a tibble you can conveniently fill. For example,
#'
#' Code like this:
#'
#' ```R
#' create_data_dictionary("loanbook_demo", package = "r2dii.dataraw") %>%
#'   datapasta::tribble_paste()
#' ```
#'
#' Outputs something like this:
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
#' You can now replace each `NA` with a corresponding `definition`.
#'
#' @param dataset A length-1 character string giving the name of a dataset.
#' @param package A length-1 character string giving the name of a package.
#'
#' @seealso [data_dictionary].
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @keywords internal
#'
#' @examples
#' create_data_dictionary("loanbook_demo", package = "r2dii.dataraw")
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
