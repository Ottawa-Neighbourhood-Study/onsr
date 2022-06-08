#' Details and geographies for Ottawa Neighbourhood Study (ONS) Gen2 boundaries.
#'
#' A dataset containing the ONS IDs, English and French names, and boundaries
#' for 111 neighbourhoods in Ottawa.
#'
#'
#' @format A simple feature geometry collection with 111 features/rows
#' and 3 fields/columns:
#' \describe{
#'   \item{ONS_ID}{Unique identifier, integer}
#'   \item{Name}{English name}
#'   \item{Name_FR}{French name}
#'   \item{geometry}{Neighbourhood boundary, multipolygon geometry}
#'   ...
#' }
#' @source \url{https://www.neighbourhoodstudy.ca/}
"ons_shp"

#' ONS neighbourhood ID numbers.
#'
#' A single-column tibble containing all ONS IDs. Useful for ensuring that all
#' ONS_IDs are represented in a given dataset, and for adding back any that are
#' missing.#'
#'
#' @format A tibble with 111 rows and one column:
#'
#' \describe{
#'   \item{ONS_ID}{Unique identifier, integer}
#'   ...
#' }
#' @source \url{https://www.neighbourhoodstudy.ca/}
"ons_ids"

#' Dissemination Block (DB) Populations, Ottawa, 2016 Census
#'
#' A dataset containing DB-level populations for all 8,016 DBs in Ottawa.
#'
#' Data is derived from the Statistics Canada 2016 Census Geographic Attribute
#' File, accessible here:
#'
#' * [https://open.canada.ca/data/en/dataset/32f1a777-9fcf-4e4a-8c66-82c66a2e76f1](https://open.canada.ca/data/en/dataset/32f1a777-9fcf-4e4a-8c66-82c66a2e76f1)
#'
#'
#' @format A tibble with two columns:
#' \describe{
#'   \item{DBUID}{Unique dissemination block identifier}
#'   \item{db_pop_2016}{DB population from 2016 census.}
#'   ...
#' }
#' @source \url{https://open.canada.ca/data/en/dataset/32f1a777-9fcf-4e4a-8c66-82c66a2e76f1}
"ottawa_db_pops_2016"



#' Statistics Canada Census GEOID Types
#'
#' A dataset with descriptions of the different kinds of geographic unique
#' identifiers (GEOUIDs) Statistics Canada (StatsCan) uses and accepts in its
#' API calls.
#'
#' StatsCan defines a number of GEOUID types that correspond to different
#' geographical units (e.g. provinces, census subdivisions, metropolitan areas).
#' This table shows all GEOUID types that StatsCan's API accepts, and shows how
#' to use a specific identifier (e.g. a dissemination block ID) and turn it into
#' an API-readable GEOUID.
#'
#' This table is pulled directly from StatsCan, and you can consult StatsCan's
#' website for more details here:
#'
#' * [https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm](https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm)
#'
#'
#' @format A tibble with seven columns:
#' \describe{
#'   \item{name}{Human-readable name of the GEOUID type.}
#'   \item{abbr}{Vintage geographic reference year abbreviation: either CENSUS, FED, or HRBR.}
#'   \item{type}{A = administrative, S = statistical.}
#'   \item{schema}{A four-digit number that identifies this type of GEOUID.}
#'   \item{geouid}{The type of GEOUID. This is often a column name in StatsCan shapefiles.}
#'   \item{example_name}{Human-readable name of an example geography of this type.}
#'   \item{example_geouid}{API-readable unique GEOUID of the human-readable example geography name.}
#'   ...
#' }
#' @source \url{https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm}
"census_geo_types"


#' Linking StatsCan Dissemination Areas to ONS Neighbourhoods
#'
#' @format A tibble with two columns:
#' \describe{
#'   \item{DAUID}{Each dissemination area's unique identifier.}
#'   \item{ONS_ID}{A numeric ID corresponding to an ONS neighbourhood.}
#'   ...
#' }
"da_to_ons_data"

#' Linking StatsCan Dissemination Blocks to ONS Neighbourhoods
#'
#' @format A tibble with two columns:
#' \describe{
#'   \item{DBUID}{Each dissemination block's unique identifier.}
#'   \item{ONS_ID}{A numeric ID corresponding to an ONS neighbourhood.}
#'   ...
#' }
"db_to_ons_data"

Name <- Name_FR<-  ONS_ID <- census_geo_types <- NULL
