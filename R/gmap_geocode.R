#' Tidy Geocoding with Google's API
#'
#' @param data A dataframe containing a column with addresses stored as characters.
#' @param var A column with addresses stored as characters.
#' @param api_key A valid API key for Google Cloud Services. See https://cloud.google.com/free/
#' @param verbose Boolean flag controlling console updates. Defaults to FALSE.
#' @param wait_nicely Boolean flag controlling API rate. Defaults to TRUE.
#'
#' @return The original dataframe with geocoded latitude and longitude in new columns lat and lng. Addresses that are invalid or not found return NA.
#' @export
gmap_geocode <- function(data, var, api_key = NA, verbose = FALSE, wait_nicely = TRUE) {
  # TODO: add default column for addresses.

  # get the addresses in a character vector
  addresses <- data %>% dplyr::pull ({{var}})
  num_addresses <- base::length(addresses)

  # make sure we found some addresses
  if (num_addresses == 0) base::stop ('No address column found, or column is empty. Please supply a column name of addresses.')

  # must give an api key
  if (base::is.na(api_key)) base::stop ("Please provide a valid Google Cloud API key. Get one for free here: https://cloud.google.com/free/")

  # we're only supposed to do 50 requests per second, as per the API terms of use:
  # https://developers.google.com/maps/documentation/geocoding/usage-and-billing
  # To be super nice, we'll only do 4000 per minute
  if (wait_nicely) base::Sys.sleep(60/4000)

  # here's the base url to the api
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"

  # set up a url object using the base url
  url <- httr::parse_url(base_url)

  # set up vectors for results
  lats <- lons <- base::rep(NA_real_, times = num_addresses)

  # loop through the addresses
  for (i in 1:num_addresses){
    # tell the url object we'd like to query that object
    url$query <- base::list("address" = addresses[[i]],
                      "key" = api_key)

    # make a url out of all this information
    url_full <- httr::build_url(url)

    # now try to get it
    q <- httr::GET(url=url_full)

    # give updates to the console if we want them
    if (verbose) {
      base::message (base::paste0("* Address: ", addresses[[i]]))
      base::message (base::paste0("   Status code: ", q$status_code))
    }

    # if we got a good response code and if we got at least one result
    if (q$status_code == 200 & base::length(httr::content(q)$results) > 0){
      # extract the latitude and longitude
      latlon <- httr::content(q)$results[[1]]$geometry$location
      lats[[i]] <- latlon$lat
      lons[[i]] <- latlon$lng
    }

  }


  return (dplyr::bind_cols(data,
                    dplyr::tibble(lat = lats, lng = lons)))
}

#' Geocode a Single Address with Google's API
#'
#' @param address A character vector with a single address.
#' @param api_key A valid API key for Google Cloud Services. See https://cloud.google.com/free/
#' @param verbose Boolean flag controlling console updates. Defaults to FALSE.
#' @param wait_nicely Boolean flag controlling API rate. Defaults to TRUE.
#'
#'
#' @return A dataframe with geocoded latitute and logitude.
#' @export
gmap_geocode_one <- function(address = "1243 Willowdale Ave, Ottawa, ON", api_key = NA, verbose = FALSE, wait_nicely = TRUE) {
  # must give an api key
  if (base::is.na(api_key)) stop ("Please provide a valid Google Cloud API key. Get one for free here: https://cloud.google.com/free/")

  # we're only supposed to do 50 requests per second, as per the API terms of use:
  # https://developers.google.com/maps/documentation/geocoding/usage-and-billing
  # To be super nice, we'll only do 4000 per minute
  if (wait_nicely) base::Sys.sleep(60/4000)


  # here's the base url to the api
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"

  # set up a url object using the base url
  url <- httr::parse_url(base_url)

  # tell the url object we'd like to query that object
  url$query <- base::list("address" = address,
                    "key" = api_key)

  # make a url out of all this information
  url_full <- httr::build_url(url)

  # now try to get it
  q <- httr::GET(url=url_full)

  # give updates to the console if we want them
  if (verbose) {
    message (paste0("* Address: ", address))
    message (paste0("   Status code: ", q$status_code))
  }

  lat <- NA
  lng <- NA
  # if we got a good response code and if we got at least one result
  if (q$status_code == 200 & base::length(httr::content(q)$results) > 0){
    # extract the latitude and longitude
    latlon <- httr::content(q)$results[[1]]$geometry$location
    lat <- latlon$lat
    lng <- latlon$lng
  }

  return (dplyr::tibble (lat=lat, lng=lng))
}
