#' Get neighbourhood population-weighted travel distances and times from DBs to facilities
#'
#' @param od_table A tidy origin-destination table with (at least) columns
#'  `DBUID`, `distance`, and `time`. Output from `valhallr::od_table()` should
#'  be accepted seamlessly.
#' @param n_closest The number of closest facilities to consider. Default is 5.
#' @param verbose Boolean. Would you like lots of updates in the console?
#'
#' @description This function calculates neighbourhood-level population-weighted
#' average travel times and distances from each dissemination block (DB) to the
#' n-nearest facilities. Each DB is linked to the single neighbourhood it
#' overlaps the most
#'
#' It works well with the function `valhallr::od_table()` from the **valhallr**
#' package.
#'
#' @return A 111-row tibble with population-weighted travel distances and times
#' for each ONS neighbourhood.
#' @export
ons_pop_weight_dbs <- function(od_table, n_closest = 5, verbose = TRUE){

  # for clean R CMD CHECK with dplyr data masking
  DBUID  <- distance <- time <- avg_dist <- db_pop_2016 <- avg_time <- weighted_dist <- ons_pop <- weighted_time <- NULL


  # basic input validation
  if (!"DBUID" %in% colnames(od_table)) stop ("Input data does not have a column named `DBUID`. This function is designed to work with DB-level data.")
  if (!"distance" %in% colnames(od_table) | (!"time" %in% colnames(od_table))) stop ("Input data does not have a column named `distance` or `time`. This function is designed to work with output from valhalr::od_table().")
  if ((n_closest < 1) | (round(n_closest) != n_closest)) stop ("Number of closest facilities must be an integer greater than 0.")

  # get closest and shortest
  if (verbose) message (paste0("Calculating average distance to ", n_closest, " closest facilities.."))
  closest <- od_table %>%
    dplyr::group_by(DBUID) %>%
    dplyr::arrange(DBUID, distance) %>%
    dplyr::slice_head(n = n_closest) %>%
    dplyr::summarise(avg_dist = mean(distance, na.rm = TRUE)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(DBUID = as.character(DBUID))

  if (verbose) message (paste0("Calculating average time to ", n_closest, " closest facilities.."))
  shortest <- od_table %>%
    dplyr::group_by(DBUID) %>%
    dplyr::arrange(DBUID, time) %>%
    dplyr::slice_head(n = n_closest) %>%
    dplyr::summarise(avg_time = mean(time, na.rm = TRUE)/60) %>%
    dplyr::distinct() %>%
    dplyr::mutate(DBUID = as.character(DBUID))

  # create table with each db', adnd's avg dist and avg time to 5 closest
  closest_shortest <- dplyr::left_join(closest, shortest, by = "DBUID")

  # get single-link indicator (SLI) that maps each DB to one and only one nbhd
  db_sli <- onsr::get_db_to_ons()

  # get 2016 DB populations, which we've stored in the onsr package
  db_pops <- onsr::ottawa_db_pops_2016 %>%
    dplyr::mutate(DBUID = as.character(DBUID))


  # get each DB census population, use the population to weight,
  # group by ONS_ID and summarise.
  if (verbose) message ("Creating population-weighted values at neighbourhood level...")
  ons_table <- db_pops %>%
    dplyr::left_join(closest_shortest, by = "DBUID") %>%
    dplyr::left_join(db_sli, by = "DBUID") %>%
    dplyr::mutate(weighted_dist = avg_dist * db_pop_2016,
                  weighted_time = avg_time * db_pop_2016) %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::mutate(ons_pop = sum(db_pop_2016, na.rm = TRUE)) %>%
    dplyr::arrange(ONS_ID) %>%
    dplyr::summarise(weighted_dist_ons = sum(weighted_dist, na.rm = TRUE)/ons_pop,
                     weighted_time_ons = sum(weighted_time, na.rm = TRUE)/ons_pop,
                     .groups = "drop") %>%
    dplyr::distinct() %>%
    dplyr::arrange(ONS_ID) %>%
    tidyr::drop_na()


  # repeat the process for Ottawa as a whole by assigning every
  # DB to ONS_ID 0. (might be a more elegant way but it works)
  if (verbose) message ("Creating Ottawa-wide values...")
  ottawa_level <- db_pops %>%
    dplyr::left_join(closest_shortest, by = "DBUID") %>%
    dplyr::mutate(weighted_dist = avg_dist * db_pop_2016,
                  weighted_time = avg_time * db_pop_2016) %>%
    dplyr::mutate(ONS_ID = 0) %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::mutate(ons_pop = sum(db_pop_2016, na.rm = TRUE)) %>%
    dplyr::arrange(ONS_ID) %>%
    dplyr::summarise(weighted_dist_ons = sum(weighted_dist, na.rm = TRUE)/ons_pop,
                     weighted_time_ons = sum(weighted_time, na.rm = TRUE)/ons_pop,
                     .groups = "drop") %>%
    dplyr::distinct() %>%
    dplyr::arrange(ONS_ID) %>%
    tidyr::drop_na()

  # put it together
  ons_table <- dplyr::bind_rows(ottawa_level,ons_table)

  return (ons_table)
}




#' Get percentage of residents within a certain travel time of facilities
#'
#' This function takes a long origin-destination (OD) table between dissemination
#' blocks (DBs) and facilities and computes, for each ONS neighbourhood and
#' Ottawa as a whole, the percent of residents within a given travel distance of
#' those amenities.
#'
#' OD tables can be generated using the Valhalla routing system and the R
#' package valhallr.
#'
#' @param walk_table A tbl_df with at minimum the columns `DBUID` and `time`,
#'        giving travel time in seconds.
#' @param minute_threshold Threshold in minutes. Defaults to 15.
#'
#' @return A tbl_df with columns `ONS_ID` and `pct_covered`
#' @export
get_pct_within_traveltime <- function(walk_table, minute_threshold = 15) {

  # for clean R CMD CHECK using dplyr
  DBUID <- time <- covered <- db_pop_2016 <- covered_pop <- total_pop <- pct_covered <- NULL

  if (!is.numeric(minute_threshold)) stop ("Please provide a numeric value for `minute_threshold`.")


  # group by DBUID, arrange in increasing order of time, get the top one (shortest),
  # then see if the shortest is under the threshold # of seconds
  pct_walking <- walk_table %>%
    tidyr::drop_na() %>%
    dplyr::group_by(DBUID) %>%
    dplyr::arrange(time) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(covered = dplyr::if_else(time < minute_threshold * 60, TRUE, FALSE)) %>%
    #filter(DBUID == 35060560005)
    dplyr::select(DBUID, covered) %>%

    dplyr::left_join(onsr::ottawa_db_pops_2016, by = "DBUID") %>%
    dplyr::mutate(DBUID = as.character(DBUID)) %>%
    dplyr::left_join(onsr::db_to_ons_data, by = "DBUID") %>%
    dplyr::mutate(covered_pop = covered * db_pop_2016) %>%
    #filter(ONS_ID == 48)
    dplyr::group_by(ONS_ID) %>%
    dplyr::summarise(total_pop = sum(db_pop_2016),
                     covered_pop = sum(covered_pop),
                     pct_covered = covered_pop/total_pop) %>%
    tidyr::drop_na() %>%
    dplyr::select(ONS_ID, pct_covered)

  # do it also just for ottawa overall, like it's one big neighbourhood
  ott_pct_walking <- walk_table %>%
    tidyr::drop_na() %>%
    dplyr::group_by(DBUID) %>%
    dplyr::arrange(time) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(covered = dplyr::if_else(time < minute_threshold * 60, 1, 0)) %>%
    dplyr::select(DBUID, covered) %>%
    dplyr::left_join(onsr::ottawa_db_pops_2016, by = "DBUID") %>%
    # mutate(DBUID = as.character(DBUID)) %>%
    # left_join(onsr::get_db_to_ons(), by = "DBUID") %>%
    dplyr::mutate(ONS_ID = 0) %>%
    dplyr::mutate(covered_pop = covered * db_pop_2016) %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::summarise(total_pop = sum(db_pop_2016),
                     covered_pop = sum(covered_pop),
                     pct_covered = covered_pop/total_pop) %>%
    tidyr::drop_na() %>%
    dplyr::select(ONS_ID, pct_covered)

  # put the neighbourhood-level and ottawa-wide values together
  pct_walking <- dplyr::bind_rows(ott_pct_walking, pct_walking)

  return (pct_walking)
}



#' Compute standard ONS statistics from a shapefile of points
#'
#' This function takes a shapefile of points that can represent any services--
#' for example, physicians, WIFI stations, or schools-- and some standard ONS
#' data inputs, and calculates five common ONS statistics:
#'
#'  * Points per Neighbourhood
#'  * Points / 1000 residents in the neighbourhood plus a 50m buffer
#'  * Average travel distance to 3 nearest points
#'  * Average travel distance to 1 nearest point
#'  * % of residents within a 15-minute walk of any point
#'
#' It needs as input a path to a long origin-destination (OD) table such as
#' can be generated using the Valhalla routing enging and `valhallr::od_table()`.
#'
#' @param point_shapefile A shapefile of points representing service locations.
#' @param name_prefix Optional - a prefix to be applied to column names.
#' @param name_postfix Optional - a postfix (?) to be applied to column names.
#' @param ons_shp Optional. An ONS shapefile. If not specified, uses internal data `onsr::ons_shp`.
#' @param ons_data ONS data. Can be acquired using `onsr::get_ons_data()`.
#' @param path_to_od_table File path to an OD table.
#' @param verbose Boolean, defaults TRUE. Would you like many updates?
#'
#' @return A tbl_df with the calculated statistics.
#' @export
#' @examples
#' \dontrun{
#' # file `wifi_walk_table.csv` computed previously using `valhallr::od_table()`
#' ons_data <- onsr::get_ons_data()
#' wifi_data <- onsr::compute_ons_values(point_shapefile = wifi_locations_nad, name_postfix = "wifi", ons_data = ons_data,
#'                                       path_to_od_table =  "outputs/wifi_walk_table.csv")
#' }
compute_ons_values <- function(point_shapefile, name_prefix = "", name_postfix = "", ons_shp = NA,
                               # ons_buffer_50m,
                               ons_data, path_to_od_table, verbose = TRUE){

  # for clean R CMD CHECK with dplyr data masking
  points_per_nbhd <- n <- pct_covered <- points_per_1000_pop <- polygon_attribute <- pop2016 <- value <- weighted_dist_ons <- x <- NULL

  if (is.na(ons_shp)){
    if (verbose) message ("Getting ONS shapefile and adding row for all of Ottawa.")
    ons_shp <- onsr::ons_shp %>%
      sf::st_transform(crs = 32189)

    ottawa_shp <- ons_shp %>%
      sf::st_union() %>%
      sf::st_as_sf() %>%
      dplyr::mutate(ONS_ID = 0,
             Name = "Ottawa",
             Name_FR = "") %>%
      dplyr::rename(geometry = x) %>%
      sf::st_transform(crs = 32189)

    ons_shp <- dplyr::bind_rows(ons_shp, ottawa_shp)

  }

  # if ons_shp is in WGS84, convert it
  if (stringr::str_detect(paste0(unlist(sf::st_crs(ons_shp)), collapse = " "), "WGS")){
    if (verbose) message ("Converting ons_shp to CRS NAD83/MTM zone 9 (32189)")
    ons_shp <- sf::st_transform(ons_shp, crs = 32189)
  }

  # if ottawa itself isn't in the shapefile, add it


  if (verbose) message("Calculating 50m buffer.")

  ons_buffer_50m <- sf::st_buffer(ons_shp, 50)


  if (verbose) message("Extracting ONS populations from data.")
  # get neighbourhood 2016 populations from ONS data
  nbhd_pop2016 <- ons_data %>%
    dplyr::filter(polygon_attribute == "pop2016") %>%
    dplyr::select(ONS_ID,
                  pop2016 = value) %>%
    dplyr::mutate(ONS_ID = as.numeric(ONS_ID))

  # create buffered version of ons_shp
  ons_buffer_50m <- sf::st_buffer(ons_shp, 50)


  # calculate some values!

  # 1. Points per Neighbourhood
  if (verbose) message("Metric 1. Points per Neighbourhood")

  points_per_nhood <- onsr::get_pts_neighbourhood(pts = point_shapefile, pgon = ons_shp) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::count() %>%
    #dplyr::summarise(num_fps = dplyr::n()) %>%
    onsr::add_back_nbhds(var = "n") %>%
    dplyr::arrange(ONS_ID) %>%
    tidyr::drop_na() %>%# exclude any docs outside of Ottawa
    dplyr::rename(points_per_nbhd = n)

  ##2. # Points / 1000 residents in the neighbourhood plus a 50m buffer
  if (verbose) message("Metric 2. # Points / 1000 residents in the neighbourhood plus a 50m buffer")

  points_per_nbhd_50m_buffer <-   {

    # get # of points in each neighbourhood plus a 50m buffer
    points_per_nbhd_50m_buffer <- onsr::get_pts_neighbourhood(pts = point_shapefile, pgon = ons_buffer_50m) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::group_by(ONS_ID) %>%
      dplyr::count() %>%
      #dplyr::summarise(num_fps = n()) %>%
      onsr::add_back_nbhds(var = "n") %>%
      dplyr::arrange(ONS_ID)

    # create new data with comparison for inspection
    points_with_buffer <- dplyr::left_join(points_per_nbhd_50m_buffer,
                                           nbhd_pop2016,
                                           by = "ONS_ID") %>%
      dplyr::mutate(points_per_nbhd_50mbuffer_per_1000_pop = (n / pop2016) * 1000) %>%
      dplyr::filter(!is.na(ONS_ID))

    points_with_buffer %>%
      dplyr::select(ONS_ID, points_per_nbhd_50mbuffer_per_1000_pop)

  }


  ## Metric 3: Average travel distance to 3 nearest points (measured from DBs and population-weighted up to the neighbourhood level)
  if (verbose) message("Metric 3: Average travel distance to 3 nearest points")

  avg_dist_3 <- {
    # read the big OD table, do population weighting, then set values to NA for
    # the two cemeteries and carleton.
    readr::read_csv(path_to_od_table) %>%
      #  dplyr::filter(cpso %in% ottawa_docs$cpso) %>%
      onsr::ons_pop_weight_dbs(n_closest = 3) %>%
      dplyr::mutate(weighted_dist_ons = dplyr::if_else(ONS_ID %in% c(5, 17, 71), NA_real_, weighted_dist_ons)) %>%
      dplyr::select(ONS_ID, avg_dist_km_3_closest = weighted_dist_ons)

  }

  ## Metric 4: Average distance to 1 nearest point
  # (measured from dissemination blocks (DBs) and population-weighted up neighbourhood level)

  if (verbose)  message("Metric 4: Average travlel distance to 1 nearest point")
  avg_dist_1 <- {
    readr::read_csv(path_to_od_table) %>%
      #  dplyr::filter(cpso %in% ottawa_docs$cpso) %>%
      onsr::ons_pop_weight_dbs(n_closest = 1) %>%
      dplyr::mutate(weighted_dist_ons = dplyr::if_else(ONS_ID %in% c(5, 17, 71), NA_real_, weighted_dist_ons)) %>%
      dplyr::select(ONS_ID, avg_dist_km_1_closest = weighted_dist_ons)
  }



  ## Metric 5: % of residents within a 15-minute walk of any point
  # NOTE! This one requires more calculation and data sources.
  # We're also using data from the ISM study here: DB-to-doc OD table
  # And statscan data: 2016 DB populations
  # And ONS data: DB-to-neighbourhood single-link indicator
  # need to filter so that we only include the docs in our input group by cpso

  if (verbose)  message("Metric 5: % of residents within a 15-minute walk of any point")

  pct_15min_walk <-  readr::read_csv(path_to_od_table) %>%
    onsr::get_pct_within_traveltime(minute_threshold=15) %>%
    dplyr::mutate(pct_covered= dplyr::if_else(ONS_ID %in% c(5, 17, 71), NA_real_, pct_covered)) %>%
    dplyr::rename(pct_15min_walk = pct_covered)


  if (verbose) message("Combining results")

  new_data <- points_per_nhood %>%
    dplyr::left_join(points_per_nbhd_50m_buffer, by = "ONS_ID") %>%
    dplyr::left_join(avg_dist_1, by = "ONS_ID") %>%
    dplyr::left_join(avg_dist_3, by = "ONS_ID") %>%
    dplyr::left_join(pct_15min_walk, by = "ONS_ID") %>%
    dplyr::left_join(sf::st_set_geometry(onsr::ons_shp, NULL) %>% dplyr::select(ONS_ID, Name), by = "ONS_ID") %>%
    dplyr::mutate(Name = dplyr::if_else(ONS_ID == 0, "Ottawa", Name)) %>%
    dplyr::select(ONS_ID, Name, dplyr::everything())


  # add postfix to column names if applicable
  if (name_postfix != ""){
    if (verbose) message ("Renaming columns with postfix")
    new_data %>%
      dplyr::rename_with(.cols = c(-ONS_ID, Name), paste0, "_", name_postfix)
  }

  return(new_data)

}
