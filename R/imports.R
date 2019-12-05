#' @importFrom rlang %||% inform warn abort
#' @importFrom glue glue
#' @importFrom dplyr mutate if_else tibble as_tibble
#' @importFrom usethis ui_todo ui_path ui_code
#' @import r2dii.utils
NULL

globalVariables(
  c(
    ".data",
    # Avoid R CMD check warning about undefined global variables (best practice
    # is to instead use `.data$ALD.Production`, `.data$AssetLocation`, etc.).
    "ALD.Production",
    "AssetLocation",
    "AssetName",
    "AverageEmissionsFactor",
    "BloombergID",
    "Bloomberg_ID",
    "BondLvlEmissionsFactor",
    "BondLvlProduction",
    "CNTRY_OF_DOMICILE",
    "CompLvlEmissionsFactor",
    "CompLvlProduction",
    "CompanyName",
    "CorpBondTicker",
    "CountryOfDomicile",
    "EmissionsFactor",
    "FreeFloatShares",
    "GrossProduction",
    "IsUltimateOwner",
    "PlantLocation",
    "Production",
    "ProductionUnits",
    "Sector",
    "Technology",
    "TotalShares",
    "Year"
  )
)
