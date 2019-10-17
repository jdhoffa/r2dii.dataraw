take_snapshot_impl <- function(x, destdir, overwrite) {
  if (!overwrite && already_exists(x, destdir)) {
    inform(glue("Skipping existing snapshot of {usethis::ui_code(x)}."))
    return(invisible(x))
  }

  path <- fs::path(destdir, glue("{x}.csv"))

  directory <- fs::path_dir(path)
  if (!fs::dir_exists(directory)) {
    fs::dir_create(directory)
  }

  is_exported_data <- x %in% ls_data()
  if (is_exported_data) {
    data <- get_data()[[x]]
  } else {
    data <- x %>%
      purrr::invoke_map() %>%
      purrr::pluck(1)
  }

  if (github_wont_render(data)) {
    # Compress
    path <- glue("{path}.gz")
  }
  if (is.data.frame(data)) vroom::vroom_write(data, path, delim = ",")

  if (is.character(data)) {
    path <- glue("{fs::path_ext_remove(path)}.txt")
    readr::write_lines(data, path)
  }

  inform_wrote(x, path)

  invisible(x)
}

inform_wrote <- function(x, destdir) {
  inform(glue("Wrote `{x}` to {ui_path(destdir)}."))
}

github_wont_render <- function(data) {
  if (is.data.frame(data)) {
    # GitHub doesn't render .csv > 512K
    approx_512k <- 6000
    return(nrow(data) > approx_512k)
  }

  FALSE
}

#' Snapshots datasets and the configuration file
#'
#' @param datasets Character vector giving the name of a file (without
#'   extension) in which to write the snapshot. It must exactly match the name
#'   of an exported dataset or of a functions which output is a datasets. `NULL`
#'   defaults to take a snapshot of all possible datasets.
#' @param destdir Character vector giving the path to the directory to write to.
#'   `NULL` defaults to the working directory.
#' @param overwrite `TRUE` overwrites existing snapshots (data and configuration
#'   file).
#' @param config Path to a configuration file.
#'
#' @family handle snapshots
#' @seealso [get_config].
#'
#' @return Returns `NULL`. It's called for its side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' library(r2dii.utils)
#'
#' # Use `config` to provide a local, custom configuration file, locally -- to
#' # affect this specific call to `take_snapshot()`
#' datasets <- "DebtMarketClimate"
#' custom_config <- r2dii.utils::example_config("config-toy.yml")
#' take_snapshot(
#'   datasets, destdir = tempdir(), overwrite = TRUE, config = custom_config
#'  )
#'
#' # Use `options(r2dii_config = <custom_config>)` to provide a custom
#' # configuration file, globally -- to affect your entire R session
#' restore <- options(r2dii_config = custom_config)
#' take_snapshot(
#'   datasets, destdir = tempdir(), overwrite = TRUE
#' )
#'
#' options(restore)
#' }
take_snapshot <- function(datasets = NULL,
                          destdir = NULL,
                          overwrite = FALSE,
                          config = get_config()) {
  if (!any(grepl("package:r2dii.dataraw", search()))) {
    abort("r2dii.dataraw must be attached with `library(r2dii.dataraw)`.")
  }

  datasets <- datasets %||% possible_snapshots()
  destdir <- destdir %||% fs::path_wd()
  if (!fs::dir_exists(destdir)) {
    fs::dir_create(destdir)
  }

  withr::with_options(list(r2dii_config = config), {
    copy_config(destdir, overwrite = overwrite, config = config)

    datasets %>%
      write_snapshots(destdir = destdir, overwrite = overwrite) %>%
      warn_snapshot_errors()
  })

  invisible(datasets)
}

copy_config <- function(destdir, overwrite, config) {
  config_file <- fs::path_file(config)
  config_path <- fs::path(destdir, config_file)
  config_already_exists <- fs::file_exists(config_path)

  if (config_already_exists && !overwrite) {
    inform(glue("Skipping existing snapshot of {ui_path(config_file)}."))
    return(invisible(destdir))
  }

  fs::file_copy(config, new_path = destdir, overwrite = TRUE)
  inform(glue("Wrote {ui_path(config_file)} to {ui_path(config_path)}."))

  invisible(destdir)
}

already_exists <- function(x, destdir) {
  existing <- function(ext, x, destdir) {
    fs::file_exists(fs::path(destdir, glue("{x}{ext}")))
  }

  c(".csv", ".csv.gz", ".txt") %>%
    purrr::map_lgl(existing, x, destdir) %>%
    any()
}

write_snapshots <- function(datasets, destdir, overwrite) {
  purrr::map(
    datasets,
    purrr::safely(take_snapshot_impl),
    destdir = destdir,
    overwrite = overwrite
  ) %>%
    purrr::set_names(datasets)
}

warn_snapshot_errors <- function(results) {
  errors <- results %>%
    purrr::map("error") %>%
    purrr::discard(is.null)

  if (length(errors) > 0) {
    errors_ <- errors %>%
      names() %>%
      glue::glue_collapse(", ")

    warn(glue(
      "Can't write the following datasets:
      {errors_}"
    ))
  }

  invisible(results)
}

#' Datasets to snapshot
#'
#' @return A character string giving the datasets to snapshot.
#' @export
#' @family handle snapshots
#'
#' @examples
#' possible_snapshots()
possible_snapshots <- function() {
  search_docs("r2dii.dataraw") %>%
    dplyr::filter(stringr::str_detect(.data$concept, "possible_snapshots")) %>%
    dplyr::pull(.data$alias)
}

# Get datasets in data/
get_data <- function() {
  mget(ls_data(), envir = as.environment("package:r2dii.dataraw"))
}

# List datasets in data/
ls_data <-  function() {
    utils::data(package = "r2dii.dataraw")$results[, "Item"]
}

search_docs <- function(packages = NULL) {
  docs <- suppressMessages(
    purrr::reduce(utils::hsearch_db(), dplyr::full_join)
  )

  result <- rlang::set_names(tibble::as_tibble(docs), tolower)

  if (is.null(packages)) {
    return(result)
  }

  dplyr::filter(result, .data$package %in% packages)

}
