# Download and create a dataframe containing StatsCan census geotypes.
# We use these geotypes to call StatsCan's API.
# This code worked as of June 15, 2021. Since it's scraping data, it may need
# to be adjusted if StatsCan updates its website.

statscan_geo_site <- rvest::read_html("https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm")

census_geo_types <- rvest::html_table(statscan_geo_site)[[4]]

census_geo_types <- dplyr::rename(census_geo_types,
                                  name = 1,
                                  abbr = 2,
                                  type = 3,
                                  schema = 4,
                                  geouid = 5,
                                  geouid_length = 6,
                                  example_name = 7,
                                  example_geouid = 8)

# remove first and last row
census_geo_types <- census_geo_types[-1,]
census_geo_types <- census_geo_types[-nrow(census_geo_types),]


# remove some footnote junk and any punctuation
census_geo_types <- dplyr::mutate(census_geo_types,
                                  geouid = stringr::str_remove_all(geouid, "[:punct:]|Table 4.*"),
                                  abbr = stringr::str_remove_all(abbr, "[:punct:]|Table 4.*"))


usethis::use_data(census_geo_types, overwrite = TRUE)
