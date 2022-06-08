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
