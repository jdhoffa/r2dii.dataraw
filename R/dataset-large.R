# Size: 18 MB ------------------------------------------------------

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' FIN.DATA()
#' }
FIN.DATA <- function() {
  read_datastore(
    glue("financial_data_{FINANCIAL.TIMESTAMP()}.csv")
  )
}

# Size: 1 MB > 10 MB ------------------------------------------------------

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.SPV()
#' }
ALD.SPV <- function() {
  data <- read_datastore("credit_methodology_asset_production.csv")
  data %>%
    dplyr::transmute(
      ALD.ID = as.character(AssetName),
      ALD.Asset.Location = as.character(AssetLocation),
      Sector = as.character(Sector),
      Year = Year,
      Technology = as.character(Technology),
      ALD.Production = Production,
      ALD.Production = dplyr::if_else(ALD.Production <=0, 0, ALD.Production),
      ALD.ProductionUnits = as.character(ProductionUnits),
      ALD.EmissionsFactor = EmissionsFactor,
      ALD.link.Level = "Asset"
    ) %>%
    set_columns_to_na_if_needed()
}

#' Dataset
#' @export
#' @family datasets in directories other than Reference or datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' Fund.Data()
#' }
Fund.Data <- function() {
  file <- glue("FundsData{FINANCIAL.TIMESTAMP()}.rda")
  path <- FIN.DATA.PATH("Fund Data", file)
  tibble::as_tibble(readr::read_rds(path))
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.Company()
#' }
ALD.Company <- function() {
  ALD.EQ()
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.EQ()
#' }
ALD.EQ <- function() {
  data <- read_datastore("MasterData_Ownership_DataStore.csv")

  # TODO: Many datasets need the same transfomation. DRY
  data %>%
    dplyr::transmute(
      ALD.ID = as.character(Bloomberg_ID),
      ALD.Location = as.character(PlantLocation),
      DomicileCountry = as.character(CNTRY_OF_DOMICILE),
      Sector = as.character(Sector),
      Technology = as.character(Technology),
      Year = as.numeric(Year),
      ALD.Production = as.numeric(CompLvlProduction),
      ALD.Production = dplyr::if_else(ALD.Production <= 0, 0, ALD.Production),
      ALD.ProductionUnits = as.character(ProductionUnits),
      ALD.EmissionsFactor = as.numeric(CompLvlEmissionsFactor),
      ALD.link.Level = "Bloomberg_ID",
      FreeFloatShares = as.numeric(FreeFloatShares),
      TotalShares = as.numeric(TotalShares),
      CompanyName = as.character(CompanyName)
    )
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.CC()
#' }
ALD.CC <- function() {
  data <- read_datastore("credit_methodology_rollup.csv")

  data %>%
    dplyr::transmute(
      ALD.ID = as.character(CompanyName),
      DomicileCountry = as.character(CountryOfDomicile),
      ALD.Location = as.character(PlantLocation),
      Sector = as.character(Sector),
      Year = as.numeric(Year),
      Technology = as.character(Technology),
      ALD.Production = as.numeric(GrossProduction),
      ALD.Production = dplyr::if_else(ALD.Production <=0, 0, ALD.Production),
      ALD.ProductionUnits = as.character(ProductionUnits),
      ALD.EmissionsFactor = as.numeric(AverageEmissionsFactor),
      ALD.link.Level = "Company",
      ALD.Company.UO.flag = IsUltimateOwner
    )
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' FundsTrusts()
#' }
FundsTrusts <- function() {
  # Legacy comment: this should probably be removed or outsourced to data store!

  FIN.DATA()[
    grepl("Fund", FIN.DATA()[["bics_subgroup"]]) |
      grepl("ETF", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Stapled Security", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Stapled Security", FIN.DATA()[["security_type"]]) |
      grepl("Unit", FIN.DATA()[["security_type"]]) |
      grepl("Unit", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Fund", FIN.DATA()[["security_type"]]) |
      grepl("ETF", FIN.DATA()[["security_type"]]) |
      grepl("Funds", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Funds", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Trust", FIN.DATA()[["bics_subgroup"]]) |
      grepl("Trust", FIN.DATA()[["security_type"]]) |
      grepl("REIT", FIN.DATA()[["bics_subgroup"]]) |
      grepl("REIT", FIN.DATA()[["security_type"]]) |
      grepl("REIT", FIN.DATA()[["icb_subsector"]]) |
      grepl("Alternative Investment", FIN.DATA()[["icb_subsector"]]) |
      grepl("Limited Partnership", FIN.DATA()[["security_type"]]),
    ]
}

#' Dataset
#' @export
#' @family datasets in datastore
#' @family possible_snapshots
#' @examples
#' \dontrun{
#' ALD.CB()
#' }
ALD.CB <- function() {
  data <- read_datastore("MasterData_Debt_DataStore.csv")

  data %>%
    dplyr::mutate(
      ALD.ID = as.character(CorpBondTicker),
      ALD.Location = as.character(PlantLocation),
      DomicileCountry = as.character(NA),
      Sector = as.character(Sector),
      Technology = as.character(Technology),
      Year = as.numeric(Year),
      ALD.Production = as.numeric(BondLvlProduction),
      ALD.Production = dplyr::if_else(ALD.Production <=0, 0, ALD.Production),
      ALD.ProductionUnits = as.character(ProductionUnits),
      ALD.EmissionsFactor = as.numeric(BondLvlEmissionsFactor),
      ALD.link.Level = "CorpBondTicker"
    )
}

# Helpers -----------------------------------------------------------------

set_columns_to_na_if_needed <- function(data) {
  # FIXME? https://github.com/2DegreesInvesting/Reference/issues/34
  names_diff <- setdiff(colnames(ALD.CC()), colnames(data))
  data[ , names_diff] <- NA

  data
}
