# functions to get and work with Statistics Canada census data

#########
# function to take StatsCan's json response and tidy it into a nice tibble
# hat tip to this github discussion for the vctr::vec_as_names() trick to
# suppress warnings: https://github.com/tidyverse/tibble/issues/632
fix_response <-function (resp) {
  col_names <- resp$COLUMNS

  result <- resp$DATA %>%
    tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(col_names, quiet = TRUE))

  result
}


#' Get 2016 Census geographies and geographic attributes for a geographic level.
#'
#' @description This function provides an interface to Statistics Canada's 2016
#'   Census geographies Web Data Service (WDS) and returns the geographic
#'   identifiers (i.e., DGUIDs) for a geographic level. These DGUIDs are useful
#'   because you can use them to query StatsCan's *other* APIs to retrieve
#'   census data.
#'
#'   Please see StatsCan's website for more information:
#'
#'   * [https://www12.statcan.gc.ca/wds-sdw/cr2016geo-eng.cfm](https://www12.statcan.gc.ca/wds-sdw/cr2016geo-eng.cfm)
#'
#' @param geos The geographic level to query. Defaults to "DA" for Dissemination
#'   Areas, but the following values are accepted:
#'
#'   * CD = Census divisions
#'   * CMACA = Census metropolitan areas and census agglomerations
#'   * CSD = Census subdivisions (municipalities)
#'   * CT = Census tracts
#'   * DA = Dissemination areas
#'   * DPL = Designated places
#'   * ER = Economic regions
#'   * FED = Federal electoral districts (2013 Representation Order)
#'   * FSA = Forward sortation areas
#'   * HR = Health regions (including LHINs and PHUs)
#'   * POPCNTR = Population centres
#'   * PR = Canada, provinces and territories
#'
#' @param cpt One province or territory code. Defaults to "35" for Ontario, but
#'   the following values are accepted:
#'
#' *  00 = All provinces and territories
#' *  10 = Newfoundland and Labrador
#' *  11 = Prince Edward Island
#' *  12 = Nova Scotia
#' *  13 = New Brunswick
#' *  24 = Quebec
#' *  35 = Ontario
#' *  46 = Manitoba
#' *  47 = Saskatchewan
#' *  48 = Alberta
#' *  59 = British Columbia
#' *  60 = Yukon
#' *  61 = Northwest Territories
#' *  62 = Nunavut
#'
#' @param lang The language in which to return results. There are two options:
#'
#'   * E = English
#'   * F = French
#'
#' @return A tibble containing parsed API results.
#' @export
#'
census_get_geographies <- function(geos = "DA", cpt = 35, lang = "E"){

  # validate the numeric parameter for province/territory
  cpt <- as.character(cpt)
  if (cpt == "0") cpt <- "00"

  if (is.na(as.numeric(cpt))) stop("Non-numeric cpt value. Please see documentation for a full list of options.")
  if (!cpt %in% c("00", "10", "11", "12", "13", "24", "35", "46", "47", "48", "59", "60", "61", "62")) stop ("Invalid cpt value. Please see documentation for a full list of options.")

  # validate geography
  if (!geos %in% c("CD", "CMACA", "CSD", "CT", "DA", "DPL", "ER", "FED", "FSA", "HR", "POPCNTR", "PR")) stop(paste0("Invalid geos value: ", geos,". Please see the documentation for a full list of options."))

  # validate language
  if (!lang %in% c("E","F")) stop (paste0("Invalid lang value: ", lang,". Please select E for English or F for French."))

  url <- paste0("https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.json?lang=",lang,"&geos=",geos,"&cpt=",cpt)

  resp <- httr::GET(url)

  if (!resp$status_code == 200) stop (paste0("Bad API reponse from request: ", resp$url))

  resp_json <- resp %>%
    httr::content(type = "text", encoding = "UTF-8")

  if (!jsonlite::validate(resp_json)) stop (paste0("API returned non-json response from request: ", resp$url, "
                                                 \nReponse begins:\n",
                                                   stringr::str_trunc(resp_json, width = 450)))

  resp_parsed <- jsonlite::fromJSON(resp_json) %>%
    fix_response()

  return(resp_parsed)

}



#' Get 2016 Census Profile data for a geography of interest.
#'
#' @description This function provides an interface to Statistics Canada's 2016
#'   Census geographies Web Data Service (WDS) and returns some or all Census
#'   Profile data for a geography of interest. Data is returned for individual
#'   regions specified by Dissemination Geography Unique Identifiers (or DGUIDs),
#'   which can correspond to census regions like dissemination areas, political
#'   regions like provinces and territories, or economic regions.
#'
#'   **NOTE:** This function is under active development, and it is my intention
#'   to turn it into a properly "tidy" function.
#'
#'   For more details on the underlying API, please see StatsCan's documentation here:
#'
#'   * [https://www12.statcan.gc.ca/wds-sdw/cpr2016-eng.cfm](https://www12.statcan.gc.ca/wds-sdw/cpr2016-eng.cfm)
#'
#' @param dguid For more details, please see [Dissemination Geography Unique Identifier: Definition and Structure](https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm)
#'
#' @param topic Which subset of census data should be returned? The following values are accepted;
#'
#' * 0 = All topics (default)
#' * 1 = Aboriginal peoples
#' * 2 = Education
#' * 3 = Ethnic origin
#' * 4 = Families, households and marital status
#' * 5 = Housing
#' * 6 = Immigration and citizenship
#' * 7 = Income
#' * 8 = Journey to work
#' * 9 = Labour
#' * 10 = Language
#' * 11 = Language of work
#' * 12 = Mobility
#' * 13 = Population
#' * 14 = Visible minority
#'
#' @param notes Should footnotes be returned? 0 = no (default), 1 = yes.
#'
#' @param stat What statistic should be returned? 0 = counts (default), 1 = rates.
#'
#' @param lang Language "EN" or "FR" (FIXME confirm!)
#'
#'
#' @return data
#' @examples
#' \dontrun{
#' # get mobility data for Ottawa, Census Division 3506
#' onsr::census_make_dguid(data = "3506",
#'                        geouid_type = "CDUID",
#'                        vintage_year = 2016) %>%
#' onsr:::census_get_data_one(topic = 12)
#' }
#' @export
census_get_data_one <- function(dguid = NA, topic = 0, notes = 0, stat = 0, lang = "E"){

  # for dguid, just check that one was supplied. checking dguid validity is complicated,
  # and if it's no good the API will tell us later.
  if (is.na(dguid)) stop("Please supply a dguid. For more information, see the documentation or Statistics Canada's website: https://www12.statcan.gc.ca/wds-sdw/cpr2016-eng.cfm")

  # make sure the other parameters are acceptable
  if (!topic %in% 0:14) stop("Invalid topic parameter. Please see documentation for valid options.")
  if (!notes %in% 0:1) stop("Invalid notes parameter. Please see documentation for valid options.")
  if (!stat %in% 0:1) stop("Invalid stat parameter. Please see documentation for valid options.")
  if (!lang %in% c("E","F")) stop("Invalid lang parameter. Please use 'E' for English and 'F' for French.")


  url <- paste0("https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json?lang=",lang,"&dguid=",dguid,"&topic=",topic,"&notes=",notes,"&stat=",stat)

  resp <- httr::GET(url)

  if (!resp$status_code == 200) stop (paste0("Bad API reponse from request: ", resp$url))

  resp_json <- resp %>%
    httr::content(type = "text", encoding = "UTF-8")

  if (!jsonlite::validate(resp_json)) stop (paste0("API returned non-json response from request: ", resp$url, "
                                                 \nReponse begins:\n",
                                                   stringr::str_trunc(resp_json, width = 450)))

  resp_parsed <- jsonlite::fromJSON(resp_json) %>%
    fix_response()

  return(resp_parsed)

}

#' Get StatsCan Census Data for Specified Regions
#'
#' @inherit census_get_data_one description
#'
#' @details *Note: This function is under active development. Please send suggestions or
#' bug reports to christopher.a.belanger (at) gmail.com.*
#'
#' @param dguids A character vector or one-column dataframe containing the
#' DGUIDs to query. *Note: in a future version this will be made "tidier."*
#' @inheritParams census_get_data_one
#'
#' @param lang Language "EN" or "FR" (FIXME confirm!)
#'
#' @return data
#' @examples
#' \dontrun{
#' # get mobility data for Ottawa and Leeds & Grenville, Census Divisions 3506 &
#' 3507.
#' onsr::census_make_dguid(data = c("3506", "3507"),
#'                        geouid_type = "CDUID",
#'                        vintage_year = 2016) %>%
#' onsr:::census_get_data(topic = 12)
#' }
#' @export
census_get_data <- function(dguids = NA, topic = 0, notes = 0, stat = 0, lang = "E"){

  # no data validation, not very tidy, works for now but fix this up
  dguids <- unlist(dguids)

  purrr::map_df(dguids, census_get_data_one, topic = topic, notes = notes, stat = stat, lang = lang)

}


#' Create DGUIDs for StatsCan Census Regions
#'
#' This function takes a set of census region geographic unique identifiers
#' (GEOUIDs) and returns their corresponding geographic identifiers (DGUIDs).
#' These DGUIDs are useful because you can use them to query StatsCan's APIs to
#'  retrieve census data.
#'
#' https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm
#'
#' @param data A vector or one-column dataframe of census Geographic Unique
#' Identifiers (GEOUIDs).
#' @param geouid_type The type of GEOUID. Consult the dataframe
#'  `onsr::census_geo_types` to see the valid options.
#' @param vintage_year The year for the data you're interested in. Different
#' GEOUID types have different options. The default is 2016, the most recent
#' census year available.
#'
#' @return Data
#' @export
census_make_dguid <- function(data, geouid_type = NA, vintage_year = 2016) {

  # for clean R CMD CHECK with dplyr data masking
  geouid <- NULL

  # validate type input
  if (is.na(geouid_type)) stop ("Please specify the type of GEOUID you are providing. See `census_geouid_types` for options.")
  if (!geouid_type %in% onsr::census_geo_types$geouid) stop ("Please supply a valid GEOUID type. Consult onsr::census_geo_types for options. ")

  # we're only checkign that it's a number, not that statscan has any results
  if (!is.numeric(vintage_year)) stop ("Please supply a numeric value for vintage_year.")

  # make sure it's a list or dataframe
  if (typeof(data) == "list"){
    if (!"data.frame" %in% class(data)) stop ("Invalid input. Please supply a vector or one-column dataframe.")
    data <- dplyr::pull(data, 1)#dplyr::rename(data, input = 1)
  }

  # if it's an atomic vector
  if (typeof(data) != "list"){
    if (!is.null(dim(data))) stop ("Invalid input. Please supply a vector or one-column dataframe.")
    data <- as.character(data)#tibble::tibble(input = as.character(data))
  }



  # get the prefix
  geouid_specs <- dplyr::filter(onsr::census_geo_types,
                                geouid == geouid_type)

  prefix <- paste0(vintage_year, geouid_specs$type, geouid_specs$schema)



  # if (type == "CSDUID") {
  #   if (any(nchar(data) != 7)) stop ("Invalid input. Census subdivision area identifiers must all be of length 7.")
  #   prefix <- paste0(vintage_year,"A0005")
  # }
  #
  # if (type == "DAUID") {
  #   if (any(nchar(data) != 8)) stop ("Invalid input. Dissemination area identifiers must all be of length 8.")
  #   prefix <- paste0(vintage_year,"S0512")
  # }
  #
  # if (type == "DBUID") {
  #   if (any(nchar(data) != 11)) stop ("Invalid input. Dissemination block identifiers must all be of length 11.")
  #   prefix <- paste0(vintage_year,"S0513")
  # }


    tibble::tibble(dguid = paste0(prefix, data))

}
