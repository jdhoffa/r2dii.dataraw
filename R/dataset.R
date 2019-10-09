#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' # Quarter 2, 2019, contains the relevant data
#' restore <- options(
#'   r2dii_config = r2dii.utils::example_config("config_2019Q2.yml")
#'  )
#'
#' LoanMarket()
#'
#' options(restore)
#' }
LoanMarket <- function() {
  file <- glue("{FINANCIAL.TIMESTAMP()}.CORP.BICS.Others.Corp.Loans.csv")
  read_csv_(CBMARKET.DATA.PATH(file), fileEncoding = "UTF-8-BOM")
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' # Quarter 2, 2019, contains the relevant data
#' restore <- options(
#'   r2dii_config = r2dii.utils::example_config("config_2019Q2.yml")
#' )
#'
#' LoanMarketClimate()
#'
#' options(restore)
#' }
LoanMarketClimate <- function() {
  file <- glue("{FINANCIAL.TIMESTAMP()}.CORP.BICS.ModelRelevant.Corp.Loans.csv")
  read_csv_(CBMARKET.DATA.PATH(file), fileEncoding = "UTF-8-BOM")
}

#' Dataset
#'
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.BV()
#' }
ALD.BV <- function() {
  data <- read_datastore("MasterData_BookValue_DataStore.csv")
  data %>%
    dplyr::transmute(
      ALD.ID = as.character(BloombergID),
      ALD.Location = as.character(PlantLocation),
      DomicileCountry = as.character(CountryOfDomicile),
      Sector = as.character(Sector),
      Technology = as.character(Technology),
      Year = as.numeric(Year),
      ALD.Production = as.numeric(GrossProduction),
      ALD.Production = dplyr::if_else(ALD.Production <=0, 0, ALD.Production),
      ALD.ProductionUnits = as.character(ProductionUnits),
      ALD.EmissionsFactor = as.numeric(AverageEmissionsFactor),
      ALD.link.Level = "Bloomberg_ID",
      CompanyName = as.character(CompanyName)
    )
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' BALANCE.SHEET.DATA()
#' }
BALANCE.SHEET.DATA <- function() {
  read_datastore("balance_sheet_data.csv")
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' EQMarket.Size()
#' }
EQMarket.Size <- function() {
  path <- EQMARKET.DATA.PATH("EQMarketSizeBasedOnIndices.csv")
  tibble::as_tibble(read_csv_(path))
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' DebtMarketClimate()
#' }
DebtMarketClimate <- function() {
  file <- glue(
    "{FINANCIAL.TIMESTAMP()}.CORP.BICS.ModelRelevant.Corp.AmtOut.csv"
  )
  read_csv_(CBMARKET.DATA.PATH(file))
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' DebtMarket()
#' }
DebtMarket <- function() {
  file <- glue("{FINANCIAL.TIMESTAMP()}.CORP.BICS.Others.Corp.AmtOut.csv")
  read_csv_(CBMARKET.DATA.PATH(file))
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' if(interactive()) PHYSICAL.RISK.EQ()
#' }
PHYSICAL.RISK.EQ <- function() {
  path <- MASTER.DATA.PATH("PortCheck_MasterData_Risk_Ownership.rda")
  tibble::as_tibble(readr::read_rds(path))
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' PHYSICAL.RISK.CB()
#' }
PHYSICAL.RISK.CB <- function() {
  path <- MASTER.DATA.PATH("PortCheck_MasterData_Risk_Debt.rda")
  tibble::as_tibble(readr::read_rds(path))
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' SCENLong()
#' }
SCENLong <- function() {
  SCEN_raw() %>%
    dplyr::filter(
      .data$Year %in% c(START.YEAR():(START.YEAR() + 5), ADDITIONAL.YEAR()),
      .data$Source %in% SCENARIO.SOURCES.LIST()
    ) %>%
    dplyr::mutate(
      Sector = dplyr::case_when(
        .data$Units == "PJ" & .data$Scenario != "B2DS" ~ "Demand",
        .data$Technology == "Coal"                     ~ "Coal",
        .data$Technology %in% c("Oil","Gas")           ~ "Oil&Gas",
        TRUE ~ .data$Sector
      )
    ) %>%
    dplyr::transmute(
      Source = as.character(.data$Source),
      ScenarioGeography = as.character(.data$ScenarioGeography),
      Scenario = as.character(.data$Scenario),
      Sector = as.character(.data$Sector),
      Technology = as.character(.data$Technology),
      Year = as.numeric(.data$Year),
      Direction = as.character(.data$Direction),
      FairSharePerc = as.numeric(.data$FairSharePerc)
    )
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' SCEN()
#' }
SCEN <- function() {
  out <- SCEN_raw() %>%
    dplyr::filter(
      .data$Year >= START.YEAR() & .data$Year <= (START.YEAR() + TIME.HORIZON()),
      .data$Source %in% SCENARIO.SOURCES.LIST()
    ) %>%
    dplyr::mutate(
      Sector = dplyr::case_when(
        .data$Units == "PJ" & .data$Scenario != "B2DS" ~ "Demand",
        .data$Technology == "Coal"                     ~ "Coal",
        .data$Technology %in% c("Oil","Gas")           ~ "Oil&Gas",
        TRUE ~ .data$Sector
      )
    ) %>%
    dplyr::transmute(
      Source = as.character(.data$Source),
      ScenarioGeography = as.character(.data$ScenarioGeography),
      Scenario = as.character(.data$Scenario),
      Sector = as.character(.data$Sector),
      Technology = as.character(.data$Technology),
      Year = as.numeric(.data$Year),
      Direction = as.character(.data$Direction),
      FairSharePerc = as.numeric(.data$FairSharePerc)
    ) %>%
    warn_if_has_duplicated_scenarios()

  out
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' SEC.TYPE.BONDS()
#' }
SEC.TYPE.BONDS <- function() {
  # Legacy comment: this should probably be removed or outsourced to data store!
  grep("Bond", unique(FIN.DATA()$security_type), value = TRUE)
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' TYPE.BONDS()
#' }
TYPE.BONDS <- function() {
  # Legacy comment: this should probably be removed or outsourced to data store!
  c(
    SEC.TYPE.BONDS(),
    "Government Non-Mort. Debt",
    "Mortgage Debt",
    "Other Collateralized Debt"
  )
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' TYPE.RECEIPTS()
#' }
TYPE.RECEIPTS <- function() {
  # Legacy comment: this should probably be removed or outsourced to data store!

  FIN.DATA()[
    grepl("Receipt", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Receipt", FIN.DATA()[["security_type"]]) |
      grepl("NY Registered Shares", FIN.DATA()[["security_type"]]) |
      grepl("NVDR", FIN.DATA()[["security_type"]]) |
      grepl("German Certificate", FIN.DATA()[["security_type"]]) |
      grepl("Depositary", FIN.DATA()[["security_type"]]) |
      grepl("Dutch Certificate", FIN.DATA()[["security_type"]]) |
      grepl("Foreign Share", FIN.DATA()[["security_type"]]) |
      grepl("CEDEAR", FIN.DATA()[["security_type"]]),
    ]
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' Receipts()
#' }
Receipts <- function() {
  # Legacy comment: this should probably be removed or outsourced to data store!

  FIN.DATA()[
    grepl("Receipt", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Receipt", FIN.DATA()[["security_type"]]) |
      grepl("NY Registered Shares", FIN.DATA()[["security_type"]]) |
      grepl("NVDR", FIN.DATA()[["security_type"]]) |
      grepl("German Certificate", FIN.DATA()[["security_type"]]) |
      grepl("Depositary", FIN.DATA()[["security_type"]]) |
      grepl("Dutch Certificate", FIN.DATA()[["security_type"]]) |
      grepl("Foreign Share", FIN.DATA()[["security_type"]]) |
      grepl("CEDEAR", FIN.DATA()[["security_type"]]),
    ]
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' CB_OG()
#' }
CB_OG <- function() {
  read_datastore("oil_and_gas_resource_type_rollup_debt.csv")
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' EQ_OG()
#' }
EQ_OG <- function() {
  read_datastore("oil_and_gas_resource_type_rollup_ownership.csv")
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' Indices()
#' }
Indices <- function() {
  # Intentionally using an implementation for an output closest to legacy (#31)
  pattern <- glue("{FINANCIAL.TIMESTAMP()}.*Index.csv")
  Indexlist <- fs::dir_ls(INDEX.DATA.PATH(), regexp = pattern)

  for (i in 1:length(Indexlist)) {
    IndexData <- use_legacy_names(read_csv_(Indexlist[i]))

    if (exists("out")) {
      out <- rbind(out, IndexData)
    } else {
      out <- IndexData
    }
  }

  out
}

use_legacy_names <- function(data) {
  rlang::set_names(data, indices_old_names())
}

# indices_old_names() helps read data in `Indices()`
#
# RE https://github.com/2DegreesInvesting/Reference/issues/31.
# On 2019-08-01 Klaus sent me (Mauro) via slack the file
# 2diiIssue_IndexFile.rda. which I named `Indices_old` and used as internal
# data.
#
# But I only ever used just names of the dataset. The rest is now discard to
# minimize the risk of exposing potentially private data.
#
# I first stored the entire dataset but I only ever used just the names. So I
# now use just the names and nothing else. This is to minimize the risk of
# exposing potentially private data.
#
# indices_old <- readr::read_rds(
#   here::here("data-raw", "private", "Indices_2019-08-01.rda")
# )
# datapasta::vector_paste(names(indices_old))
indices_old_names <- function() {
  c(
    "Ticker",
    "Name",
    "Asset.Class",
    "Weight....",
    "Price",
    "Shares",
    "Market.Value",
    "Notional.Value",
    "Sector",
    "SEDOL",
    "ISIN",
    "Exchange",
    "Country",
    "Currency",
    "Market.Currency",
    "Fte",
    "PortfolioName",
    "Market"
  )
}

warn_if_has_duplicated_scenarios <- function(data) {
  if (has_duplicated_scenarios(data)) {
    warn(glue(
      "The set of scenarios is not unique per sector and technology and \\
      will create duplicates"
    ))
  }

  invisible(data)
}

has_duplicated_scenarios <- function(data) {
  distinct_scenarios <- dplyr::summarise(
    data,
    dplyr::n_distinct(
      .data$Scenario,
      .data$Year,
      .data$ScenarioGeography,
      .data$Sector,
      .data$Technology
    )
  )

  distinct_scenarios != nrow(data)
}

SCEN_raw <- function() {
  file <- glue("Scenarios_AnalysisInput_{START.YEAR()}.csv")
  data <- read_csv_(SCENARIO.DATA.PATH(file))

  data %>%
    tibble::as_tibble() %>%
    dplyr::filter(
      !(.data$Technology %in% c("HydroCap","NuclearCap") & .data$Units == "PJ")
    )
}
