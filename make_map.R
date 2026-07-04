# animated_tracks_map.R

packages_needed <- c(
  "data.table",
  "lubridate",
  "stringr",
  "plotly",
  "htmlwidgets",
  "ggplot2",
  "gganimate",
  "gifski"
)

packages_missing <- packages_needed[
  !packages_needed %in% installed.packages()[, "Package"]
]

if (length(packages_missing) > 0) {
  install.packages(packages_missing, repos = "https://cloud.r-project.org")
}

library(data.table)
library(lubridate)
library(stringr)
library(plotly)
library(htmlwidgets)
library(ggplot2)
library(gganimate)
library(gifski)

# ------------------------------------------------------------
# Einstellungen
# ------------------------------------------------------------

data_dir <- data_dir <- "C:/Users/piuss/Hochschule Rhein-Main/Vorlesungen/4-Datenmanagement,-analyse,-visualisierung/Ausarbeitung Grotemeier/Github_Repository/dein-repo/data"
output_dir <- "C:/Users/piuss/Hochschule Rhein-Main/Vorlesungen/4-Datenmanagement,-analyse,-visualisierung/Ausarbeitung Grotemeier/Github_Repository/dein-repo/output"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "animated_gbfs_tracks_map.html")

local_tz <- "America/Vancouver"
gap_threshold_minutes <- 7.5

# ------------------------------------------------------------
# Dateien finden
# ------------------------------------------------------------

files <- list.files(
  data_dir,
  pattern = "^vancouver_gbfs_vehicle_locations_.*\\.csv$",
  full.names = TRUE
)

files <- sort(files)

if (length(files) == 0) {
  stop("Keine CSV-Dateien im data-Ordner gefunden.")
}

message("Gefundene Dateien: ", length(files))

# ------------------------------------------------------------
# Zeit aus Dateiname lesen
# ------------------------------------------------------------

extract_time_from_filename <- function(file) {
  filename <- basename(file)
  
  raw_time <- stringr::str_match(
    filename,
    "vancouver_gbfs_vehicle_locations_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})\\.csv"
  )[, 2]
  
  clean_time <- stringr::str_replace(
    raw_time,
    "^(\\d{4}-\\d{2}-\\d{2})_(\\d{2})-(\\d{2})-(\\d{2})$",
    "\\1 \\2:\\3:\\4"
  )
  
  lubridate::force_tz(
    lubridate::ymd_hms(clean_time),
    tzone = local_tz
  )
}

# ------------------------------------------------------------
# Einzelne Datei einlesen
# ------------------------------------------------------------

read_one_file <- function(file) {
  dt <- data.table::fread(file, showProgress = FALSE)
  dt <- data.table::as.data.table(dt)
  
  file_time_local <- extract_time_from_filename(file)
  
  dt[, source_file := basename(file)]
  dt[, snapshot_time_vancouver := file_time_local]
  dt[, snapshot_time_utc := lubridate::with_tz(file_time_local, "UTC")]
  
  if (!"vehicle_key" %in% names(dt)) {
    dt[, vehicle_key := NA_character_]
  }
  
  if (!"vehicle_id" %in% names(dt)) {
    dt[, vehicle_id := NA_character_]
  }
  
  if (!"provider" %in% names(dt)) {
    dt[, provider := NA_character_]
  }
  
  dt[
    is.na(vehicle_key) & !is.na(provider) & !is.na(vehicle_id),
    vehicle_key := paste(provider, vehicle_id, sep = "_")
  ]
  
  dt
}

# ------------------------------------------------------------
# Daten laden
# ------------------------------------------------------------

dt <- data.table::rbindlist(
  lapply(files, read_one_file),
  fill = TRUE
)

dt[, lat := suppressWarnings(as.numeric(lat))]
dt[, lon := suppressWarnings(as.numeric(lon))]

dt <- dt[
  !is.na(lat) &
    !is.na(lon) &
    lat >= -90 &
    lat <= 90 &
    lon >= -180 &
    lon <= 180 &
    !is.na(vehicle_key) &
    vehicle_key != ""
]

# Grober räumlicher Filter für Vancouver
dt <- dt[
  lat >= 49.15 &
    lat <= 49.35 &
    lon >= -123.30 &
    lon <= -122.95
]

dt <- unique(
  dt,
  by = c("provider", "vehicle_key", "lat", "lon", "snapshot_time_vancouver")
)

setorder(dt, provider, vehicle_key, snapshot_time_vancouver)

# ------------------------------------------------------------
# Zeitlücken erkennen und Liniensegmente bilden
# ------------------------------------------------------------

dt[
  ,
  previous_time := shift(snapshot_time_vancouver),
  by = .(provider, vehicle_key)
]

dt[
  ,
  gap_minutes := as.numeric(
    difftime(snapshot_time_vancouver, previous_time, units = "mins")
  )
]

dt[
  ,
  new_segment := fifelse(
    is.na(gap_minutes) | gap_minutes <= gap_threshold_minutes,
    0L,
    1L
  )
]

dt[
  ,
  segment_id := cumsum(new_segment) + 1L,
  by = .(provider, vehicle_key)
]

# ------------------------------------------------------------
# Linien-Daten vorbereiten
# ------------------------------------------------------------

line_dt <- dt[
  ,
  .SD[order(snapshot_time_vancouver)],
  by = .(provider, vehicle_key, segment_id)
]

line_dt[
  ,
  line_group := paste(provider, vehicle_key, segment_id, sep = "_")
]

# Nur Linien mit mindestens zwei verschiedenen Punkten
valid_lines <- line_dt[
  ,
  .(
    n_points = .N,
    n_unique_positions = uniqueN(paste(lat, lon))
  ),
  by = line_group
][
  n_points >= 2 & n_unique_positions >= 2
]

line_dt <- line_dt[line_group %in% valid_lines$line_group]

# ------------------------------------------------------------
# Animations-Daten vorbereiten
# ------------------------------------------------------------

dt[
  ,
  frame_time := format(snapshot_time_vancouver, "%Y-%m-%d %H:%M:%S")
]

dt[
  ,
  popup := paste0(
    "Provider: ", provider,
    "<br>Fahrzeug: ", vehicle_key,
    "<br>Zeit Vancouver: ", frame_time,
    "<br>Lat: ", round(lat, 6),
    "<br>Lon: ", round(lon, 6)
  )
]

center_lat <- mean(dt$lat, na.rm = TRUE)
center_lon <- mean(dt$lon, na.rm = TRUE)


for (current_frame in frame_labels) {
  current_points <- dt_gif_plot[dt_gif_plot$frame_label == current_frame, ]
  
  p <- base_plot +
    geom_point(
      data = current_points,
      aes(
        x = lon,
        y = lat,
        color = provider
      ),
      size = 1.8,
      alpha = 0.9
    ) +
    labs(
      subtitle = paste("Zeit:", current_frame)
    )
  
  print(p)
  Sys.sleep(0.35)
}


# ------------------------------------------------------------
# Karte erstellen
# ------------------------------------------------------------

fig <- plot_ly()

# Hintergrund-Linien einzeichnen
if (nrow(line_dt) > 0) {
  fig <- fig %>%
    add_trace(
      data = line_dt,
      type = "scattermapbox",
      mode = "lines",
      lat = ~lat,
      lon = ~lon,
      split = ~line_group,
      color = ~provider,
      line = list(width = 2),
      opacity = 0.35,
      hoverinfo = "none",
      showlegend = FALSE
    )
}

# Animierte Punkte einzeichnen
fig <- fig %>%
  add_trace(
    data = dt,
    type = "scattermapbox",
    mode = "markers",
    lat = ~lat,
    lon = ~lon,
    frame = ~frame_time,
    color = ~provider,
    text = ~popup,
    hoverinfo = "text",
    marker = list(
      size = 10,
      opacity = 0.9
    )
  ) %>%
  layout(
    title = "Animierte GBFS-Bewegungen mit Fahrzeuglinien",
    mapbox = list(
      style = "carto-positron",
      center = list(
        lat = center_lat,
        lon = center_lon
      ),
      zoom = 12
    ),
    margin = list(l = 0, r = 0, t = 50, b = 0),
    legend = list(
      orientation = "h",
      x = 0.02,
      y = 0.98,
      bgcolor = "rgba(255,255,255,0.8)"
    )
  ) %>%
  animation_opts(
    frame = 900,
    transition = 250,
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Zeit Vancouver: "
    )
  ) %>%
  animation_button(
    x = 0.05,
    y = 0,
    xanchor = "left",
    yanchor = "bottom"
  )

# ------------------------------------------------------------
# Speichern
# ------------------------------------------------------------

htmlwidgets::saveWidget(
  fig,
  file = output_file,
  selfcontained = TRUE
)

message("Fertig.")
message("Animierte Karte gespeichert unter: ", output_file)