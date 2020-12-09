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
geocode_gmap <- function(data, var, api_key = NA, verbose = FALSE, wait_nicely = TRUE) {
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
geocode_gmap_one <- function(address = "1243 Willowdale Ave, Ottawa, ON", api_key = NA, verbose = FALSE, wait_nicely = TRUE) {
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



#' Tidy Geocoding with the City of Ottawa's API
#'
#' @param data A dataframe containing a column with addresses stored as
#'   characters.
#' @param var A column with addresses stored as characters.
#' @param verbose Boolean flag controlling console updates. Defaults to FALSE.
#' @param wait_nicely Boolean flag controlling API rate. Defaults to TRUE.
#'
#' @description Geocode a column of addresses using the City of Ottawa's
#'   geocoding service. No API key is needed, and the service uses ESRI's
#'   technology under the hood. Consult
#'   \href{https://open.ottawa.ca/pages/developer-resources}{the City of
#'   Ottawa's developer resources} or
#'   \href{https://developers.arcgis.com/rest/services-reference/geocode-addresses.htm}{ESRI's
#'   API documentation} for more information.
#'
#' @return The original dataframe with geocoded latitude and longitude in new
#'   columns lat and lng. Addresses that are invalid or not found return NA.
#' @export
geocode_ottawa <- function(data, var, verbose = FALSE, wait_nicely = TRUE) {
  # TODO: add default column for addresses.
  # TODO: do batching for >1000 addresses.

  # get the addresses in a one-column tibble
<<<<<<< HEAD
  addresses <- data %>% dplyr::select (address = {{var}})
=======
  addresses <- data %>% dplyr::select ({{var}})
>>>>>>> 52f1aec328a85fc57995d2ef2f8d0bcba0e7642f

  # ESRI doesn't like prefixed apartment numbers or units, so we remove them
  # we also remove anything after a comma: we only want the street name
  # we're only searching within Ottawa
  addresses <- addresses %>%
    dplyr::mutate(address = base::gsub(x = address, pattern = "^\\d*\\w*-", replacement = ""),
                  address = base::gsub(x = address, pattern = ",.*", replacement = ""))

  # create json in the right format for the api
  # we create a temp column so we can nest, then we remove it
  # then we use purrr::map to make lists of the right kind (couldn't get nested tibbles to work)
  # convert to json, then trim the first and last character
  json_addresses <- addresses %>%
    dplyr::mutate(temp_for_json = 1:nrow(addresses)) %>%
    dplyr::mutate( attributes =
                     purrr::map2(temp_for_json,
                                 address,
                                 function(x,y) list(OBJECTID = jsonlite::unbox(x),
                                                    SingleLine = jsonlite::unbox(y)))) %>%
    dplyr::select(attributes) %>%
    tidyr::nest(records = attributes) %>%
    jsonlite::toJSON() %>%
    base::as.character()

  json_addresses <- json_addresses %>%
    base::substr(2, (nchar(json_addresses)-1))

  base_url <- "https://maps.ottawa.ca/arcgis/rest/services/compositeLocator/GeocodeServer/geocodeAddresses"

  # set up a url object using the base url
  url <- httr::parse_url(base_url)

  # make a url out of all this information
  url_full <- httr::build_url(url)

  # now try to get it
  q <- httr::POST(url=url_full,
                  body =  base::list("addresses" = json_addresses,
                                     "f" = "json",
                                     "outSR" = "4326"))

  # if we got a good response code
  # extract the geocoded location
  if (q$status_code == 200){

    latlon <- httr::content(q) %>%
      jsonlite::fromJSON()

    lng <- latlon$locations$location$x
    lat <- latlon$locations$location$y
  }

  # combine our results back with the original input data
  # convert NaNs to NAs to be consistent with Google Maps' responses
  results <- dplyr::bind_cols(
    data,
    dplyr::tibble(lat = lat, lng = lng) %>%
      dplyr::mutate(lat = dplyr::if_else(base::is.nan(lat), NA_real_, lat),
                    lng= dplyr::if_else(base::is.nan(lng), NA_real_, lng))
  )

  return(results)
}



# add these variables as NULL so R CMD CHECK won't throw notes about them
latlon <- address <- temp_for_json <- NULL

