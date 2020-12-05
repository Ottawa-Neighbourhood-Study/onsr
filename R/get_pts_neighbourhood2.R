# function to get the neighbourhood each point is in
get_pts_neighbourhood2 <- function(pts, pgon = ons_shp){
  # check input validity
  if (!"sf" %in% class(pts)) stop("Invalid input: pts must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")
  if (!"sf" %in% class(pgon)) stop("Invalid input: pgon must have class 'sf', e.g. a shapefile loaded with sf::read_sf().")

  # make sure the two datasets have the same CRS
  if (sf::st_crs(pts) != sf::st_crs(pgon)) pts <- sf::st_transform(pts, sf::st_crs(pgon))

  # do a spatial join. #
  results <- sf::st_join(pts, pgon)

  return (results)
}
