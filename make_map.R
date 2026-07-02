# make_map.R
# Erstellt:
# - output/gbfs_tracks_map.html
# - output/collection_gaps.csv
# - output/vehicle_gaps.csv
# - output/snapshot_overview.csv

packages_needed <- c(
  "data.table",
  "dplyr",
  "lubridate",
  "stringr",
  "sf",
  "leaflet",
  "htmlwidgets",
  "htmltools"
)

packages_missing <- packages_needed[
  !packages_needed %in% installed.packages()[, "Package"]
]

if (length(packages_missing) > 0) {
  install.packages(packages_missing, repos = "https://cloud.r-project.org")
}

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# ------------------------------------------------------------
# Einstellungen
# ------------------------------------------------------------

data_dir <- "data"
output_dir <- "output"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_pattern <- "^vancouver_gbfs_vehicle_locations_.*\\.csv$"

# Erwartet sind ca. 5 Minuten Abstand.
# Alles darüber wird als Lücke dokumentiert.
gap_threshold_minutes <- 7.5

local_tz <- "America/Vancouver"

map_file <- file.path(output_dir, "gbfs_tracks_map.html")
collection_gaps_file <- file.path(output_dir, "collection_gaps.csv")
vehicle_gaps_file <- file.path(output_dir, "vehicle_gaps.csv")
snapshot_overview_file <- file.path(output_dir, "snapshot_overview.csv")

# ------------------------------------------------------------
# Hilfsfunktionen
# ------------------------------------------------------------

parse_time_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(with_tz(x, "UTC"))
  }
  
  x_chr <- as.character(x)
  
  parsed <- suppressWarnings(ymd_hms(x_chr, tz = "UTC", quiet = TRUE))
  
  missing_idx <- is.na(parsed)
  if (any(missing_idx)) {
    parsed[missing_idx] <- suppressWarnings(
      ymd_hm(x_chr[missing_idx], tz = "UTC", quiet = TRUE)
    )
  }
  
  parsed
}

extract_local_time_from_filename <- function(file) {
  base <- basename(file)
  
  timestamp_raw <- str_match(
    base,
    "vancouver_gbfs_vehicle_locations_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})\\.csv"
  )[, 2]
  
  if (is.na(timestamp_raw)) {
    return(as.POSIXct(NA))
  }
  
  timestamp_clean <- str_replace(
    timestamp_raw,
    "^(\\d{4}-\\d{2}-\\d{2})_(\\d{2})-(\\d{2})-(\\d{2})$",
    "\\1 \\2:\\3:\\4"
  )
  
  # Dateiname wurde bei dir aus Vancouver-Ortszeit erzeugt.
  force_tz(
    ymd_hms(timestamp_clean, quiet = TRUE),
    tzone = local_tz
  )
}

read_one_snapshot <- function(file) {
  dt <- fread(file, showProgress = FALSE)
  
  dt[, source_file := basename(file)]
  
  if (!"snapshot_time_utc" %in% names(dt)) {
    file_time_local <- extract_local_time_from_filename(file)
    dt[, snapshot_time_utc := with_tz(file_time_local, "UTC")]
  } else {
    dt[, snapshot_time_utc := parse_time_utc(snapshot_time_utc)]
  }
  
  if (!"snapshot_time_vancouver" %in% names(dt)) {
    dt[, snapshot_time_vancouver := with_tz(snapshot_time_utc, local_tz)]
  } else {
    parsed_local <- suppressWarnings(
      ymd_hms(as.character(snapshot_time_vancouver), tz = local_tz, quiet = TRUE)
    )
    
    missing_idx <- is.na(parsed_local)
    
    if (any(missing_idx)) {
      parsed_local[missing_idx] <- with_tz(
        snapshot_time_utc[missing_idx],
        local_tz
      )
    }
    
    dt[, snapshot_time_vancouver := parsed_local]
  }
  
  if (!"vehicle_key" %in% names(dt)) {
    dt[, vehicle_key := NA_character_]
  }
  
  if (!"vehicle_id" %in% names(dt)) {
    dt[, vehicle_id := NA_character_]
  }
  
  if (!"provider" %in% names(dt)) {
    dt[, provider := NA_character_]
  }
  
  # Fallback, falls vehicle_key fehlt.
  dt[
    is.na(vehicle_key) & !is.na(provider) & !is.na(vehicle_id),
    vehicle_key := paste(provider, vehicle_id, sep = "_")
  ]
  
  dt
}

format_local <- function(x) {
  format(with_tz(x, local_tz), "%Y-%m-%d %H:%M:%S %Z")
}

make_linestring_safe <- function(lon, lat) {
  coords <- cbind(lon, lat)
  
  coords <- coords[
    !is.na(coords[, 1]) &
      !is.na(coords[, 2]) &
      is.finite(coords[, 1]) &
      is.finite(coords[, 2]),
    ,
    drop = FALSE
  ]
  
  if (nrow(coords) < 2) {
    return(NULL)
  }
  
  if (nrow(unique(coords)) < 2) {
    return(NULL)
  }
  
  st_linestring(coords)
}

# ------------------------------------------------------------
# Dateien einlesen
# ------------------------------------------------------------

files <- list.files(
  data_dir,
  pattern = file_pattern,
  full.names = TRUE
)

if (length(files) == 0) {
  stop("Keine passenden CSV-Dateien in 'data/' gefunden.")
}

message("Lese ", length(files), " Snapshot-Dateien ...")

raw <- rbindlist(
  lapply(files, read_one_snapshot),
  fill = TRUE
)

required_cols <- c("provider", "vehicle_key", "vehicle_id", "lat", "lon", "snapshot_time_utc")

missing_required <- setdiff(required_cols, names(raw))

if (length(missing_required) > 0) {
  stop(
    "Diese benötigten Spalten fehlen: ",
    paste(missing_required, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Daten bereinigen
# ------------------------------------------------------------

dt <- copy(raw)

dt[, lat := suppressWarnings(as.numeric(lat))]
dt[, lon := suppressWarnings(as.numeric(lon))]

dt <- dt[
  !is.na(snapshot_time_utc) &
    !is.na(lat) &
    !is.na(lon) &
    lat >= -90 &
    lat <= 90 &
    lon >= -180 &
    lon <= 180
]

# Doppelte Zeilen entfernen
dt <- unique(
  dt,
  by = c(
    "provider",
    "vehicle_key",
    "vehicle_id",
    "lat",
    "lon",
    "snapshot_time_utc"
  )
)

dt[, snapshot_time_vancouver := with_tz(snapshot_time_utc, local_tz)]

setorder(dt, snapshot_time_utc, provider, vehicle_key)

if (nrow(dt) == 0) {
  stop("Nach der Bereinigung sind keine gültigen Fahrzeugpositionen übrig.")
}

# ------------------------------------------------------------
# Snapshot-Übersicht und globale Sammellücken
# ------------------------------------------------------------

snapshot_overview <- dt[
  ,
  .(
    vehicles = .N,
    providers = paste(sort(unique(provider)), collapse = ", "),
    source_files = paste(sort(unique(source_file)), collapse = ", ")
  ),
  by = snapshot_time_utc
][order(snapshot_time_utc)]

snapshot_overview[, snapshot_time_vancouver := with_tz(snapshot_time_utc, local_tz)]
snapshot_overview[, previous_snapshot_time_utc := shift(snapshot_time_utc)]
snapshot_overview[, gap_minutes := as.numeric(
  difftime(snapshot_time_utc, previous_snapshot_time_utc, units = "mins")
)]

snapshot_overview_export <- copy(snapshot_overview)
snapshot_overview_export[, snapshot_time_utc := format(snapshot_time_utc, "%Y-%m-%d %H:%M:%S UTC")]
snapshot_overview_export[, snapshot_time_vancouver := format_local(snapshot_time_vancouver)]
snapshot_overview_export[, previous_snapshot_time_utc := format(previous_snapshot_time_utc, "%Y-%m-%d %H:%M:%S UTC")]

fwrite(snapshot_overview_export, snapshot_overview_file)

collection_gaps <- snapshot_overview[
  !is.na(gap_minutes) & gap_minutes > gap_threshold_minutes,
  .(
    gap_start_utc = previous_snapshot_time_utc,
    gap_end_utc = snapshot_time_utc,
    gap_start_vancouver = with_tz(previous_snapshot_time_utc, local_tz),
    gap_end_vancouver = with_tz(snapshot_time_utc, local_tz),
    gap_minutes = round(gap_minutes, 2)
  )
]

collection_gaps_export <- copy(collection_gaps)

if (nrow(collection_gaps_export) > 0) {
  collection_gaps_export[, gap_start_utc := format(gap_start_utc, "%Y-%m-%d %H:%M:%S UTC")]
  collection_gaps_export[, gap_end_utc := format(gap_end_utc, "%Y-%m-%d %H:%M:%S UTC")]
  collection_gaps_export[, gap_start_vancouver := format_local(gap_start_vancouver)]
  collection_gaps_export[, gap_end_vancouver := format_local(gap_end_vancouver)]
}

fwrite(collection_gaps_export, collection_gaps_file)

# ------------------------------------------------------------
# Fahrzeugbezogene Lücken
# ------------------------------------------------------------

track_dt <- dt[
  !is.na(vehicle_key) &
    vehicle_key != ""
]

setorder(track_dt, provider, vehicle_key, snapshot_time_utc)

track_dt[
  ,
  previous_time_utc := shift(snapshot_time_utc),
  by = .(provider, vehicle_key)
]

track_dt[
  ,
  previous_lat := shift(lat),
  by = .(provider, vehicle_key)
]

track_dt[
  ,
  previous_lon := shift(lon),
  by = .(provider, vehicle_key)
]

track_dt[
  ,
  vehicle_gap_minutes := as.numeric(
    difftime(snapshot_time_utc, previous_time_utc, units = "mins")
  )
]

vehicle_gaps <- track_dt[
  !is.na(vehicle_gap_minutes) & vehicle_gap_minutes > gap_threshold_minutes,
  .(
    provider,
    vehicle_key,
    vehicle_id,
    gap_start_utc = previous_time_utc,
    gap_end_utc = snapshot_time_utc,
    gap_start_vancouver = with_tz(previous_time_utc, local_tz),
    gap_end_vancouver = with_tz(snapshot_time_utc, local_tz),
    gap_minutes = round(vehicle_gap_minutes, 2),
    previous_lat,
    previous_lon,
    next_lat = lat,
    next_lon = lon
  )
]

vehicle_gaps_export <- copy(vehicle_gaps)

if (nrow(vehicle_gaps_export) > 0) {
  vehicle_gaps_export[, gap_start_utc := format(gap_start_utc, "%Y-%m-%d %H:%M:%S UTC")]
  vehicle_gaps_export[, gap_end_utc := format(gap_end_utc, "%Y-%m-%d %H:%M:%S UTC")]
  vehicle_gaps_export[, gap_start_vancouver := format_local(gap_start_vancouver)]
  vehicle_gaps_export[, gap_end_vancouver := format_local(gap_end_vancouver)]
}

fwrite(vehicle_gaps_export, vehicle_gaps_file)

# ------------------------------------------------------------
# Liniensegmente erzeugen
# Wichtig:
# Bei Lücken wird die Linie getrennt, damit keine falsche Verbindung entsteht.
# ------------------------------------------------------------

track_dt[
  ,
  new_segment := fifelse(
    is.na(vehicle_gap_minutes) | vehicle_gap_minutes <= gap_threshold_minutes,
    0L,
    1L
  )
]

track_dt[
  ,
  segment_id := cumsum(new_segment) + 1L,
  by = .(provider, vehicle_key)
]

line_parts <- track_dt[
  ,
  {
    ord <- order(snapshot_time_utc)
    
    lon_ordered <- lon[ord]
    lat_ordered <- lat[ord]
    time_ordered <- snapshot_time_utc[ord]
    
    line <- make_linestring_safe(lon_ordered, lat_ordered)
    
    if (is.null(line)) {
      NULL
    } else {
      .(
        vehicle_id = vehicle_id[ord][1],
        n_points = length(time_ordered),
        first_time_utc = min(time_ordered, na.rm = TRUE),
        last_time_utc = max(time_ordered, na.rm = TRUE),
        geometry = list(line)
      )
    }
  },
  by = .(provider, vehicle_key, segment_id)
]

if (nrow(line_parts) > 0) {
  lines_sf <- st_sf(
    line_parts[, .(
      provider,
      vehicle_key,
      vehicle_id,
      segment_id,
      n_points,
      first_time_utc,
      last_time_utc
    )],
    geometry = st_sfc(line_parts$geometry, crs = 4326)
  )
  
  lines_sf$popup <- paste0(
    "<b>Fahrzeuglinie</b><br>",
    "Provider: ", htmlEscape(lines_sf$provider), "<br>",
    "Vehicle Key: ", htmlEscape(lines_sf$vehicle_key), "<br>",
    "Vehicle ID: ", htmlEscape(lines_sf$vehicle_id), "<br>",
    "Segment: ", lines_sf$segment_id, "<br>",
    "Punkte: ", lines_sf$n_points, "<br>",
    "Von: ", format_local(lines_sf$first_time_utc), "<br>",
    "Bis: ", format_local(lines_sf$last_time_utc)
  )
} else {
  lines_sf <- NULL
}

# ------------------------------------------------------------
# Aktuelle Standorte: letzter Snapshot
# ------------------------------------------------------------

latest_snapshot_time <- max(dt$snapshot_time_utc, na.rm = TRUE)

latest_points <- dt[
  snapshot_time_utc == latest_snapshot_time
]

latest_points_sf <- st_as_sf(
  latest_points,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

latest_points_sf$popup <- paste0(
  "<b>Letzter bekannter Standort</b><br>",
  "Provider: ", htmlEscape(latest_points_sf$provider), "<br>",
  "Vehicle Key: ", htmlEscape(latest_points_sf$vehicle_key), "<br>",
  "Vehicle ID: ", htmlEscape(latest_points_sf$vehicle_id), "<br>",
  "Lat: ", round(latest_points_sf$lat, 6), "<br>",
  "Lon: ", round(latest_points_sf$lon, 6), "<br>",
  "Zeit Vancouver: ", format_local(latest_points_sf$snapshot_time_utc)
)

# ------------------------------------------------------------
# Karte bauen
# ------------------------------------------------------------

providers_all <- sort(unique(dt$provider))

provider_palette <- colorFactor(
  palette = c(
    "#1b9e77",
    "#d95f02",
    "#7570b3",
    "#e7298a",
    "#66a61e",
    "#e6ab02",
    "#a6761d",
    "#666666"
  ),
  domain = providers_all
)

min_time <- min(dt$snapshot_time_utc, na.rm = TRUE)
max_time <- max(dt$snapshot_time_utc, na.rm = TRUE)

summary_html <- paste0(
  "<div style='background:white; padding:10px; border-radius:6px; ",
  "box-shadow:0 1px 5px rgba(0,0,0,0.35); font-size:13px;'>",
  "<b>GBFS Vancouver</b><br>",
  "Zeitraum:<br>",
  format_local(min_time), "<br>",
  "bis<br>",
  format_local(max_time), "<br><br>",
  "Letzter Snapshot:<br>",
  format_local(latest_snapshot_time), "<br><br>",
  "Fahrzeuge im letzten Snapshot: ",
  nrow(latest_points), "<br>",
  "Snapshot-Dateien: ",
  length(files), "<br>",
  "Sammellücken &gt; ",
  gap_threshold_minutes,
  " min: ",
  nrow(collection_gaps), "<br>",
  "Fahrzeuglücken &gt; ",
  gap_threshold_minutes,
  " min: ",
  nrow(vehicle_gaps), "<br><br>",
  "Berichte:<br>",
  "collection_gaps.csv<br>",
  "vehicle_gaps.csv",
  "</div>"
)

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addControl(
    html = summary_html,
    position = "topright"
  )

if (!is.null(lines_sf) && nrow(lines_sf) > 0) {
  m <- m %>%
    addPolylines(
      data = lines_sf,
      color = ~provider_palette(provider),
      weight = 2,
      opacity = 0.45,
      smoothFactor = 1,
      group = "Fahrzeuglinien",
      popup = ~popup
    )
}

m <- m %>%
  addCircleMarkers(
    data = latest_points_sf,
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = ~provider_palette(provider),
    fillColor = ~provider_palette(provider),
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 1,
    group = "Letzte Standorte",
    popup = ~popup
  ) %>%
  addLegend(
    position = "bottomright",
    pal = provider_palette,
    values = providers_all,
    title = "Provider"
  ) %>%
  addLayersControl(
    overlayGroups = c("Fahrzeuglinien", "Letzte Standorte"),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(
  m,
  file = map_file,
  selfcontained = TRUE
)

message("Fertig.")
message("Karte gespeichert unter: ", map_file)
message("Sammellücken gespeichert unter: ", collection_gaps_file)
message("Fahrzeuglücken gespeichert unter: ", vehicle_gaps_file)
message("Snapshot-Übersicht gespeichert unter: ", snapshot_overview_file)