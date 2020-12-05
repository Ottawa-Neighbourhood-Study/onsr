#' Load ONS Gen 2 shapefile
#'
#' Load and validate the ONS Gen 2 shapefile from the City of Ottawa's "Open Ottawa" server.
#'
#' @param all_details Boolean. Default is FALSE. If TRUE, returns extra data about each
#' neighbourhood. Usually not required.
#'
#' @return Simple feature collection with ONS ID number, English name, and French name for 111 ONS Gen2 neighbourhoods.
#' @export
#'
#' @examples
#' \dontrun{ons_shp <- get_ons_shp()}
get_ons_shp <- function(all_details = FALSE) {
  url <- "https://opendata.arcgis.com/datasets/32fe76b71c5e424fab19fec1f180ec18_0.geojson"
  ons_shp <- sf::read_sf(url) %>%
    sf::st_make_valid()

  if (!all_details){
    ons_shp <- ons_shp %>%
      dplyr::select(ONS_ID, Name, Name_FR)
  }

  return (ons_shp)
}
