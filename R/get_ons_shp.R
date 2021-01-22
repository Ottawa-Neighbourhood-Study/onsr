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


#' Get ONS Neighbourhood Data
#'
#' @param tidy The "tidy" flag determines whether it's returned as a long tibble (default) or a wide one.
#'
#' @return A tibble with all ONS data, including metadata.
#' @export
#'
#' @examples
#' \dontrun{ons_data <- get_ons_data()}
get_ons_data <- function(tidy = TRUE){
  # set so it doesn't ask for auth
  googlesheets4::gs4_deauth()

  # load the ons neighbourhood names & ids etc.
  ons_neighbourhoods <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qfQud-_Pq3Aa9Eqetx-w46fcdFHBdtYA37Y4-xHMeO4/")

  # get the ONS names. note that right now this isn't used for anything!
  # ons_names <- ons_neighbourhoods %>%
  #   janitor::clean_names() %>%
  #   select(ons_id, name) %>%
  #   mutate(ons_id = as.character(ons_id)) %>%
  #   add_row(ons_id = "0", name = "Ottawa") %>%
  #   add_row(ons_id = "category1", name = "Neighbourhood Name")

  # get the ons data from the "wide" format on the google doc.
  # do some wrangling to get it into a useable tibble with numeric values
  ons_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qfQud-_Pq3Aa9Eqetx-w46fcdFHBdtYA37Y4-xHMeO4/",
                                        range="Data") %>%
    dplyr::rename_with(function(x) paste0("onsid_", x), -id) %>%
    dplyr::mutate(dplyr::across(where(is.list), unlist)) %>%
    dplyr::mutate(dplyr::across(-id, as.numeric))

  # get the data labels
  # there are some empty rows, which we drop with drop_na()
  # then we need to deal with columns with missing values
  # these show up as list-columns with NULL entries, so we can't just unlist
  # instead we need to replace the NULLs with " ", then unlist them into regular columns, then remove trailing whitespace
  data_labels <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qfQud-_Pq3Aa9Eqetx-w46fcdFHBdtYA37Y4-xHMeO4/",
                                           range="Layers") %>%
    janitor::clean_names() %>%
    tidyr::drop_na(category1) %>%
    dplyr::mutate(dplyr::across(where(is.list),
                  function(x) paste0(x, " "))) %>%
    dplyr::mutate(dplyr::across(where(is.list), unlist)) %>%
    dplyr::mutate(dplyr::across(everything(), stringr::str_trim))

  # put the labels together with the data, to make it easier to search
  all_data <- data_labels %>%
    dplyr::left_join(ons_data, by = c("polygon_attribute" = "id"))

  # if the tidy flag is set, pivot longer so we return a long tibble. otherwise we'll return a wide one.
  if (tidy) {
    all_data <- all_data %>%
      tidyr::pivot_longer(cols = starts_with("onsid_"),
                   names_prefix = "onsid_",
                   values_to = "value",
                   names_to = "ONS_ID") %>%
      dplyr::select(polygon_attribute, ONS_ID, value, everything())

  }

  return (all_data)
}


