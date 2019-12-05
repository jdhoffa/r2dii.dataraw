#' Dataset to bridge from country to all existing bench regions it is in
#'
#' @family datasets in Reference
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   library(dplyr)
#'
#'   BENCH.REGIONS()
#'
#'   # The `CountryISO` of Namibia is the literal string "NA"
#'   BENCH.REGIONS() %>%
#'     filter(Country == "Namibia")
#' }
BENCH.REGIONS <- function() {
  # C:\Users\Mauro\Dropbox (2° Investing)\2° Investing Team\People\Klaus\
  # GitHub\Reference\ReferenceData\BenchRegions.rda
  path <- path_reference("ReferenceData", "BenchRegions.rda")
  readr::read_rds(path) %>%
    mutate(
      CountryISO = if_else(
        .data$Country == "Namibia" & is.na(.data$CountryISO),
        "NA",
        .data$CountryISO
      )
    ) %>%
    as_tibble()
}

#' Dataset to identify the equity index regions based on the MSCI factsheets
#'
#' Dataset to identify the equity index regions based on the MSCI factsheets,
#' e.g. 23 countries within the MSCI World.
#'
#' @family datasets in Reference
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   INDEX.REGIONS()
#' }
INDEX.REGIONS <- function() {
  # Online at https://github.com/2DegreesInvesting/Reference/blob/master/...
  # ...ReferenceData/IndexRegions.rda
  path <- path_reference("ReferenceData", "IndexRegions.rda")
  IndexRegions_raw <- as_tibble(readr::read_rds(path))
  IndexRegions_raw %>%
    mutate(
      CountryISO = if_else(
        .data$Country == "Namibia" & is.na(.data$CountryISO),
        "NA",
        .data$CountryISO
      )
    ) %>%
    purrr::modify_if(is.factor, as.character)
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   sector.bridge()
#' }
sector.bridge <- function() {
  # Online at https://github.com/2DegreesInvesting/Reference/blob/master/...
  # ...ReferenceData/OldSectorBridge.csv
  path <- path_reference("ReferenceData", "OldSectorBridge.csv")
  read_csv_(path)
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   SectorBridge()
#' }
SectorBridge <- function() {
  # Online at https://github.com/2DegreesInvesting/Reference/blob/master/...
  # ...ReferenceData/SectorBridge.csv
  path <- path_reference("ReferenceData", "SectorBridge.csv")
  read_csv_(path)
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   BicsSectorBridge()
#' }
BicsSectorBridge <- function() {
  # Online at https://github.com/2DegreesInvesting/Reference/blob/master/...
  # ...DataPreparation/BICS_old%20to%20new_bridge.csv
  path <- path_reference("DataPreparation", "BICS_old to new_bridge.csv")
  read_csv_(path)
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family possible_snapshots
#'
#' @export
#' @examples
#' if (dropbox_exists()) {
#'   RevenueSplit()
#' }
RevenueSplit <- function() {
  # Online at https://github.com/2DegreesInvesting/Reference/blob/master/...
  # ...DataPreparation/ClimateSectorDups_proxy_RevenueSplit.csv
  path <- path_reference(
    "DataPreparation", "ClimateSectorDups_proxy_RevenueSplit.csv"
  )
  read_csv_(path)
}

# TODO ASK Klaus if this is the best (most stable) local path to Reference/
# Warning: Currently these datasets are read from Klaus' copy of the Reference
# repository on 2dii's Dropbox folder. This is fragile and should change to
# reading the data from a more stable directory.
path_reference <- function(...) {
  r2dii.utils::path_dropbox_2dii(
    glue("2{degrees()} Investing Team"),
    "People",
    "Klaus",
    "GitHub",
    "Reference",
    ...
  )
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family datasets that stand alone
#' @family possible_snapshots
#'
#' @export
#' @examples
#' TYPE.EQUITY()
TYPE.EQUITY <- function() {
  c(
    "Common Stock",
    "Common Stocks",
    "Tracking Stock",
    "Preferred Shares",
    "Preference"
  )
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family datasets that stand alone
#' @family possible_snapshots
#'
#' @export
#' @examples
#' GROUPS.GOVT()
GROUPS.GOVT <- function() {
  c(
    "Sovereign",
    "Sovereign Agency",
    "Municipal",
    "Municipal-City",
    "Municipal-County",
    "Multi-National",
    "Regional",
    "Regional(state/provnc)",
    "Export/Import Bank",
    "Regional Authority",
    "Regional Agencies"
  )
}

#' Dataset that should probably be removed or outsourced to data store
#'
#' @family datasets in Reference to remove or outsource to data store
#' @family datasets that stand alone
#' @family possible_snapshots
#'
#' @export
#' @examples
#' TYPE.OTHERS()
TYPE.OTHERS <- function() {
  c("Warrant", "Right")
}
