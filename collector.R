# collector.R

packages_needed <- c(
  "jsonlite",
  "dplyr",
  "tibble",
  "purrr",
  "readr",
  "lubridate",
  "tidyr"
)

packages_missing <- packages_needed[
  !packages_needed %in% installed.packages()[, "Package"]
]

if (length(packages_missing) > 0) {
  install.packages(packages_missing, repos = "https://cloud.r-project.org")
}

library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
library(tidyr)

save_dir <- "data"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

gbfs_systems <- tibble(
  provider = c("mobi_vancouver", "lime_vancouver"),
  gbfs_url = c(
    "https://gbfs.kappa.fifteen.eu/gbfs/2.2/mobi/en/gbfs.json",
    "https://data.lime.bike/api/partners/v2/gbfs/vancouver_bc/gbfs.json"
  )
)

read_gbfs_json <- function(url) {
  jsonlite::fromJSON(url, flatten = TRUE)
}

extract_feeds <- function(gbfs_obj) {
  if (!is.null(gbfs_obj$data$feeds)) {
    return(as_tibble(gbfs_obj$data$feeds))
  }
  
  data_names <- names(gbfs_obj$data)
  
  for (nm in data_names) {
    if (!is.null(gbfs_obj$data[[nm]]$feeds)) {
      return(as_tibble(gbfs_obj$data[[nm]]$feeds))
    }
  }
  
  stop("Keine Feeds in der GBFS-Datei gefunden.")
}

get_feed_url <- function(feeds, feed_name) {
  result <- feeds %>%
    filter(name == feed_name) %>%
    pull(url)
  
  if (length(result) == 0) return(NA_character_)
  result[[1]]
}

get_col_chr <- function(df, colname) {
  if (colname %in% names(df)) as.character(df[[colname]]) else rep(NA_character_, nrow(df))
}

get_col_num <- function(df, colname) {
  if (colname %in% names(df)) suppressWarnings(as.numeric(df[[colname]])) else rep(NA_real_, nrow(df))
}

get_col_logical <- function(df, colname) {
  if (colname %in% names(df)) as.logical(df[[colname]]) else rep(NA, nrow(df))
}

read_vehicle_locations <- function(provider, gbfs_url) {
  message("Lade GBFS für: ", provider)
  
  gbfs <- read_gbfs_json(gbfs_url)
  feeds <- extract_feeds(gbfs)
  
  free_bike_status_url <- get_feed_url(feeds, "free_bike_status")
  vehicle_status_url   <- get_feed_url(feeds, "vehicle_status")
  
  vehicle_feed_url <- dplyr::coalesce(free_bike_status_url, vehicle_status_url)
  
  if (is.na(vehicle_feed_url)) {
    warning("Kein free_bike_status/vehicle_status-Feed gefunden für: ", provider)
    return(tibble())
  }
  
  vehicle_raw <- read_gbfs_json(vehicle_feed_url)
  
  if (!is.null(vehicle_raw$data$bikes)) {
    vehicles <- as_tibble(vehicle_raw$data$bikes)
  } else if (!is.null(vehicle_raw$data$vehicles)) {
    vehicles <- as_tibble(vehicle_raw$data$vehicles)
  } else {
    warning("Keine data$bikes oder data$vehicles gefunden für: ", provider)
    return(tibble())
  }
  
  if (nrow(vehicles) == 0) {
    warning("Der Fahrzeug-Feed ist leer für: ", provider)
    return(tibble())
  }
  
  fetched_at_utc <- now(tzone = "UTC")
  fetched_at_local <- with_tz(fetched_at_utc, tzone = "America/Vancouver")
  
  vehicles %>%
    mutate(
      provider = provider,
      source_gbfs_url = gbfs_url,
      source_vehicle_feed_url = vehicle_feed_url,
      gbfs_version = gbfs$version,
      fetched_at_utc = fetched_at_utc,
      fetched_at_vancouver = fetched_at_local,
      feed_last_updated_utc = as_datetime(vehicle_raw$last_updated, tz = "UTC"),
      
      vehicle_id = coalesce(
        get_col_chr(vehicles, "bike_id"),
        get_col_chr(vehicles, "vehicle_id"),
        get_col_chr(vehicles, "id")
      ),
      
      lat = get_col_num(vehicles, "lat"),
      lon = get_col_num(vehicles, "lon"),
      vehicle_type_id = get_col_chr(vehicles, "vehicle_type_id"),
      is_reserved = get_col_logical(vehicles, "is_reserved"),
      is_disabled = get_col_logical(vehicles, "is_disabled"),
      current_range_meters = get_col_num(vehicles, "current_range_meters"),
      station_id = get_col_chr(vehicles, "station_id"),
      last_reported = get_col_num(vehicles, "last_reported")
    ) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(
      vehicle_key = if_else(
        !is.na(vehicle_id),
        paste(provider, vehicle_id, sep = "_"),
        NA_character_
      )
    ) %>%
    transmute(
      provider,
      vehicle_key,
      vehicle_id,
      lat,
      lon,
      vehicle_type_id,
      is_reserved,
      is_disabled,
      current_range_meters,
      station_id,
      last_reported,
      fetched_at_utc,
      fetched_at_vancouver,
      feed_last_updated_utc,
      gbfs_version,
      source_gbfs_url,
      source_vehicle_feed_url
    )
}

collect_snapshot <- function() {
  message("Starte Snapshot: ", now(tzone = "America/Vancouver"))
  
  snapshot_time_utc <- now(tzone = "UTC")
  snapshot_time_vancouver <- with_tz(snapshot_time_utc, tzone = "America/Vancouver")
  
  snapshot <- pmap_dfr(gbfs_systems, read_vehicle_locations)
  
  if (nrow(snapshot) == 0) {
    warning("Snapshot enthält keine Fahrzeuge.")
    return(invisible(NULL))
  }
  
  snapshot <- snapshot %>%
    mutate(
      snapshot_time_utc = snapshot_time_utc,
      snapshot_time_vancouver = snapshot_time_vancouver
    )
  
  timestamp_file <- format(snapshot_time_vancouver, "%Y-%m-%d_%H-%M-%S")
  
  file_name <- paste0(
    "vancouver_gbfs_vehicle_locations_",
    timestamp_file,
    ".csv"
  )
  
  file_path <- file.path(save_dir, file_name)
  
  write_csv(snapshot, file_path)
  
  message("Gespeichert: ", file_path)
  invisible(snapshot)
}

collect_snapshot()