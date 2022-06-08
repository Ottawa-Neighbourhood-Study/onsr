#' Locate points within ONS neighbourhoods.
#'
#' @description Take an sf geometry object of points and match them with ONS neighbourhoods.
#' This function is really just a convenience wrapper around a spatial join with useful defaults
#' input validation. As such, it can also be used with arbitrary polygons.
#'
#' @param pts A simple feature collection class sf containing point data that you would like to locate within polygons (by default, ONS neighbourhoods).
#' @param pgon A simple feature collection of class sf containing polygons (by default, ONS neighbourhoods).
#'
#' @return A simple feature collection of class sf with all columns in pts plus additional columns for each point's corresponding polygon (if applicable).
#' @export
#'
get_pts_neighbourhood <- function(pts, pgon = onsr::ons_shp){
  # make sure user gives inputs
  if (missing(pts)) stop ('Argument "pts" is missing. Which points would you like to locate?')
 # if (missing(pgon)) stop ('Argument "pgon" is missing. Consider running: ons_shp <- get_ons_shp()')

  # check input validity
  if (!"sf" %in% class(pts)) stop("Invalid input: pts must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")
  if (!"sf" %in% class(pgon)) stop("Invalid input: pgon must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")

  # make sure the two datasets have the same CRS
  if (sf::st_crs(pts) != sf::st_crs(pgon)) pts <- sf::st_transform(pts, sf::st_crs(pgon))

  # do a spatial join. #
  results <- sf::st_join(pts, pgon)

  return (results)
}



#' Single-Link Indicator from Dissemination Blocks (DBs) to ONS Neighbourhoods
#'
#' @description Census data is often available at the DB level, whereas ONS
#'   analyses generally take place at the neighbourhood level. This function
#'   returns an SLI that maps each DB in Ottawa to the single ONS neighbourhood
#'   it overlaps the most. The SLI was generated with a simple spatial join from
#'   StatsCan's official DB shapefile and the ONS neighbourhood boundaries,
#'   using the function sf::st_join(largest = TRUE).
#'
#'   See also `onsr::get_da_to_ons()`, which maps dissemination areas to ONS
#'   neighbourhoods.
#'
#' @return A tibble matching each Ottawa DB to one ONS neighbourhood.
#' @export
get_db_to_ons <- function() {
  onsr::db_to_ons_data %>%
    tibble::as_tibble()
}


#' Single-Link Indicator from Dissemination Areas (DAs) to ONS Neighbourhoods
#'
#' @description Census data is often available at the DA level, whereas ONS
#'   analyses generally take place at the neighbourhood level. This function
#'   returns an SLI that maps each DA in Ottawa to the single ONS neighbourhood
#'   it overlaps the most. The SLI was generated with a simple spatial join from
#'   StatsCan's official DA shapefile and the ONS neighbourhood boundaries,
#'   using the function sf::st_join(largest = TRUE).
#'
#'   See also `onsr::get_db_to_ons()`, which maps dissemination blocks to ONS
#'   neighbourhoods.
#'
#' @return A tibble matching each Ottawa DA to one ONS neighbourhood.
#' @export
get_da_to_ons <- function() {
  onsr::da_to_ons_data %>%
    tibble::as_tibble()
}




#' Add Back Missing Neighbourhoods to a Long Tibble
#'
#' @description This function takes a long tibble with a column for ONS IDs and
#'   a column for count data and adds rows for any missing ONS IDs. This is
#'   useful after you do a spatial join, e.g. using `onsr::get_pts_neighbourhood()`,
#'   and then do a count with `dplyr::summarise()`, since any neighbourhoods not
#'   represented in the input data won't get a row in the output.
#'
#'
#' @param data A tibble containing counts of points in neighbourhoods, with
#'   fewer than 111 rows. In other words, some neighbourhoods are missing.
#' @param var  The column containing the count data.
#' @param na_to_zero Boolean: Should NA values be converted to 0?
#'
#' @return A 111-row tibble
#' @importFrom rlang :=
#' @export
#' @examples
#' \dontrun{
#' # example using fictitious data
#' ons_shp <- onsr::get_ons_shp()
#' neighbourhood_counts <- onsr::get_pts_neighbourhood(pts = point_data, pgon = ons_shp) %>%
#'   sf::st_set_geometry(NULL) %>%
#'   dplyr::group_by(ONS_ID) %>%
#'   dplyr::summarise(num = n()) %>%
#'   onsr::add_back_nbhds(var = "num")
#' }
add_back_nbhds <- function(data, var, na_to_zero = TRUE){
  .temp <- .placeholder <-

  # basic input validation
  if(! var %in% colnames(data)) stop ("Missing var: please specify column name of missing values.")

  # get the full list of ons ids, add a placeholder column
  ons_ids <- onsr::ons_ids %>%
    tibble::as_tibble() %>%
    dplyr::mutate (.placeholder = NA)

  # do the full join to add back the missing ONS_IDs
  result <- data %>%
    dplyr::full_join(ons_ids, by = "ONS_ID") %>%
    dplyr::select(-.placeholder)

  # if we want to, change NA to 0 for the neighbourhoods that were missing
  if (na_to_zero) {
    result <- result %>%
      dplyr::rename(.temp = {{var}}) %>%
      dplyr::mutate( .temp = dplyr::if_else(is.na(.temp), 0, as.numeric(.temp))) %>%
      dplyr::rename({{var}} := .temp)
  }

  return (result)
}
