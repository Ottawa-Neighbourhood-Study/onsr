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
get_pts_neighbourhood <- function(pts, pgon = ons_shp){
  # make sure user gives inputs
  if (missing(pts)) stop ('Argument "pts" is missing. Which points would you like to locate?')
  if (missing(pgon)) stop ('Argument "pgon" is missing. Consider running: ons_shp <- get_ons_shp()')

  # check input validity
  if (!"sf" %in% class(pts)) stop("Invalid input: pts must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")
  if (!"sf" %in% class(pgon)) stop("Invalid input: pgon must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")

  # make sure the two datasets have the same CRS
  if (sf::st_crs(pts) != sf::st_crs(pgon)) pts <- sf::st_transform(pts, sf::st_crs(pgon))

  # do a spatial join. #
  results <- sf::st_join(pts, pgon)

  return (results)
}