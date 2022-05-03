#' Create skims file
#'
#'
make_skims <- function(bg_from_0, skim_walk, skim_bike, skim_auto, skim_tran) {

  mat <- expand_grid(fromId = bg_from_0$GEOID, toId = bg_from_0$GEOID)  |>
    left_join(skim_auto) |>
    left_join(skim_walk) |>
    left_join(skim_bike) |>
    left_join(skim_tran) |>
    rename(origin = fromId, destination = toId)



 file <- "data/skims.omx"
 if(!dir.exists(dirname(file))) dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
 response <- write_all_omx(mat, file = file, long = TRUE)

 return(file)
}


renumber_bg <- function(bg){
  bg |>
    arrange(GEOID) %>%
    mutate(omxid = row_number() - 1)
}

#' Get walk skim
#'
#'
get_walk_skim <- function(r5dir, zones){
  r5r_core <- r5r::setup_r5(r5dir, verbose = FALSE)

  tt <- travel_time_matrix(
    r5r_core,
    sf2pts(zones),
    sf2pts(zones),
    mode = "WALK",
    time_window = 1L,
    percentiles = 50L,
    breakdown = FALSE,
    breakdown_stat = "MEAN",
    max_walk_dist = 10000,
    max_trip_duration = 120L,
    walk_speed = 3.6,
    n_threads = Inf,
    verbose = FALSE,
    progress = TRUE
  ) |>
    rename(
      walk = travel_time
    ) |>
    as_tibble()


  stop_r5(r5r_core)
  return(tt)
}

#' Get bike skim
#'
#'
get_bike_skim <- function(r5dir, zones){
  r5r_core <- r5r::setup_r5(r5dir, verbose = FALSE)

  tt <- travel_time_matrix(
    r5r_core,
    sf2pts(zones),
    sf2pts(zones),
    mode = "BICYCLE",
    time_window = 1L,
    percentiles = 50L,
    breakdown = FALSE,
    breakdown_stat = "MEAN",
    max_bike_dist = Inf,
    max_trip_duration = 120L,
    bike_speed = 12,
    n_threads = Inf,
    verbose = FALSE,
    progress = TRUE
  ) |>
    rename(
      bike = travel_time
    ) |>
    as_tibble()


  stop_r5(r5r_core)
  return(tt)
}


#' Get auto skim
#'
#'
get_auto_skim <- function(r5dir, zones, periods){
  r5r_core <- r5r::setup_r5(r5dir, verbose = FALSE)

  tt <-  lapply(periods, function(period) {
    travel_time_matrix(
    r5r_core,
    sf2pts(zones),
    sf2pts(zones),
    mode = "CAR",
    departure_datetime = period,
    time_window = 1L,
    percentiles = 50L,
    breakdown = FALSE,
    breakdown_stat = "MEAN",
    max_trip_duration = 240L,
    n_threads = Inf,
    verbose = FALSE,
    progress = TRUE
  ) |>
    rename(
      auto = travel_time
    ) |>
    as_tibble()
  })

  stop_r5(r5r_core)

  tt |>
    bind_rows(.id = "period") |>
    pivot_wider(names_from = period, values_from = auto, names_prefix = "auto_")
}




#' Get transit skim
#'
#'
get_tran_skim <- function(r5dir, zones, periods) {
  r5r_core <- r5r::setup_r5(r5dir, verbose = FALSE)

  tt <- lapply(periods, function(period) {
    travel_time_matrix(
      r5r_core,
      sf2pts(zones),
      sf2pts(zones),
      mode = "TRANSIT",
      departure_datetime = period,
      time_window = 60L,
      percentiles = 50L,
      breakdown = TRUE,
      breakdown_stat = "MEAN",
      max_trip_duration = 240L,
      n_threads = Inf,
      verbose = FALSE,
      progress = TRUE
    ) |>
      # remove paths that never actually use transit
      filter(n_rides > 0) |>
      as_tibble()
  }) |>
    bind_rows(.id = "period")

  stop_r5(r5r_core)

  pivot_wider(tt, names_from = "period", values_from = c(contains("time"), "n_rides", "routes"))
}

#' Make an sf object into an r5points
#'
#' @param sf
#'
sf2pts <- function(sf){
  suppressWarnings(
    sf |>
      st_centroid() |>
      transmute(
        id = GEOID,
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      ) |>
      st_set_geometry(NULL))
}

