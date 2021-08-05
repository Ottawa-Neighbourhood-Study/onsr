#' Get Business Names & Addresses from Yelp
#'
#' To support the Ottawa Neighbourhood Study in refreshing its food availability
#' database, this function does a perfunctory Yelp search for a given term in a
#' given location.
#'
#' Results include business names and addresses. This function could be improved
#' but it works for what we need now, and Yelp is very protective of its data.
#' Be warned that scraping too much too quickly may get your IP address blocked.
#'
#' @param search_term Character string to search Yelp for.
#' @param location Character string containing the location to search. Defaults
#'   to "Ottawa, ON".
#' @param num_pages The number of pages to search. Defaults to 24.
#'
#' @return A tibble with Yelp search results.
#' @export
#'
#' @examples
#' \dontrun{
#' convenience_stores <- scrape_yelp("convenience stores")
#' }
scrape_yelp <- function(search_term, location = "Ottawa, ON", num_pages = 24){
  # SCRAPING YELP... AGAIN
  categories <- NULL

  find_desc <- urltools::url_encode(search_term)
  find_loc <- urltools::url_encode(location)

  base_url <- paste0("https://www.yelp.ca/search?find_desc=",find_desc,"&find_loc=",find_loc,"&ns=1&start=")

  page_num <- 0
  all_urls <- NULL
  # GET ALL THE LINKS and BASIC STORE DETAILS

  store_details <- tibble::tibble()

  for (page_num in 0:(num_pages-1)) {
    message (sprintf("Page %s/%s", page_num+1, num_pages)) #paste0("Page ",page_num+1, "/24"))
    url <- paste0(base_url, page_num*10)

    html <- httr::GET(url) %>%
      httr::content()

    # do it with more detailed data
    # get the json. we extract all the internal application/json scripts,
    # take the first one,
    # convert to html text,
    # remove leading and trailing junk,
    # and read it as json.
    json_data <- html %>%
      #html_elements(css = "[data-hypernova-key]")  %>%
      rvest::html_elements(css = '[type*="application/json"]') %>%
      purrr::pluck(1) %>%
      rvest::html_text() %>%
      stringr::str_remove_all("<!--|-->") %>%
      jsonlite::fromJSON()

    # then we root out the useful stuff
    hover_card_data <- json_data$legacyProps$searchAppProps$searchPageProps$searchMapProps$hovercardData

    # then we parse it
    stores <- hover_card_data %>%
      purrr::map_df( function(x) {  tibble::enframe(x) %>%
          tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
          dplyr::mutate(categories =
                          purrr::map(categories,
                                     function(x) {
                                       purrr::pluck(x, "title") %>%
                                         stringr::str_flatten(collapse = ", ")
                                     })
          )}
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), purrr::map,
                           function(x) {
                             as.character(x) %>%
                               stringr::str_flatten(collapse = ", ")
                           })
      ) %>%
      tidyr::unnest(cols = dplyr::everything())

    store_details <-
      dplyr::bind_rows(store_details, stores)

    # do a precautionary pause if we have more pages to scrape
    if (page_num < num_pages - 1) Sys.sleep(10)
  }

  return (store_details)
}
