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


Name <- Name_FR<-  ONS_ID <- NULL
