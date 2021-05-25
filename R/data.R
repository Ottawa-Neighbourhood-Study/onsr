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


Name <- Name_FR<-  ONS_ID <- NULL
