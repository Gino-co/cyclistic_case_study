# 01_clean.R — reinforced cleaning and feature engineering
# Objective: combine and clean 12 months of Divvy/Cyclistic data with validations and new features

# 0) Options
options(readr.show_col_types = FALSE, dplyr.summarise.inform = FALSE)

# 1) Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
})

# 2) Configuration
TZ <- "America/Chicago"
raw_dir <- "data/raw"
out_dir <- "data/processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 3) List files
files <- list.files(raw_dir, pattern = "\\d{4}_\\d{2}\\.csv$", full.names = TRUE) |> sort()
if (length(files) == 0) stop("No CSV files found in data/raw with pattern YYYY_MM.csv")
message("Detected files:")
writeLines(basename(files))

# 4) Robust file reading
read_one <- function(path){
  readr::read_csv(
    path,
    col_types = cols(
      ride_id = col_character(),
      rideable_type = col_character(),
      started_at = col_datetime(format = ""),
      ended_at   = col_datetime(format = ""),
      start_station_name = col_character(),
      start_station_id   = col_character(),
      end_station_name   = col_character(),
      end_station_id     = col_character(),
      start_lat = col_double(),
      start_lng = col_double(),
      end_lat   = col_double(),
      end_lng   = col_double(),
      member_casual = col_character()
    ),
    na = c("", "NA", "null", "Null", "NULL")
  ) |>
    clean_names() |>
    mutate(
      started_at = with_tz(started_at, tzone = TZ),
      ended_at   = with_tz(ended_at,   tzone = TZ),
      source_file = basename(path)
    )
}

# 5) Read and combine all files
message("Reading and combining files...")
df_raw <- purrr::map_dfr(files, read_one, .id = "file_id")

# 6) Basic normalization
message("Normalizing columns...")
df <- df_raw |>
  mutate(
    rideable_type = tolower(rideable_type),
    member_casual = case_when(
      tolower(member_casual) == "member" ~ "member",
      tolower(member_casual) == "casual" ~ "casual",
      TRUE ~ NA_character_
    ),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    date = as_date(started_at, tz = TZ),
    month = floor_date(date, "month"),
    week  = floor_date(date, "week", week_start = 1),
    dow_num = wday(date, week_start = 1),
    day_of_week = wday(date, week_start = 1, label = TRUE, abbr = FALSE),
    hour = hour(started_at),
    is_weekend = dow_num >= 6
  )

# 7) Preliminary metrics
n0 <- nrow(df)
dup_ids <- df |>
  count(ride_id, name = "n") |>
  filter(n > 1) |>
  pull(ride_id)
if (length(dup_ids) > 0) message(sprintf("Duplicate ride IDs detected: %s", length(dup_ids)))

# 8) Distance and speed if coordinates exist
geo_cols <- c("start_lat","start_lng","end_lat","end_lng")
has_geo <- all(geo_cols %in% names(df))
if (has_geo) {
  r_earth <- 6371.0088
  to_rad <- function(deg) deg * pi / 180
  df <- df |>
    mutate(
      dlat = to_rad(end_lat - start_lat),
      dlon = to_rad(end_lng - start_lng),
      a = sin(dlat/2)^2 + cos(to_rad(start_lat)) * cos(to_rad(end_lat)) * sin(dlon/2)^2,
      c = 2 * atan2(sqrt(a), sqrt(1 - a)),
      distance_km = r_earth * c,
      speed_kmh = if_else(ride_length > 0, distance_km / (ride_length/60), NA_real_)
    ) |>
    select(-dlat, -dlon, -a, -c)
}

# 9) Quality rules and filters
message("Applying quality rules...")
df <- df |>
  filter(!is.na(ride_id)) |>
  distinct(ride_id, .keep_all = TRUE) |>
  filter(!is.na(started_at) & !is.na(ended_at)) |>
  filter(ride_length > 1 & ride_length < 1440) |>
  filter(!is.na(member_casual) & !is.na(rideable_type))

if (has_geo) {
  df <- df |>
    filter(is.na(speed_kmh) | speed_kmh <= 60)
}

# 10) Post-filter counts
n1 <- nrow(df)
message(sprintf("Initial rows: %s | After cleaning: %s | Removed: %s (%.1f%%)",
                scales::comma(n0), scales::comma(n1), scales::comma(n0-n1),
                100*(n0-n1)/n0))

# 11) Hard validations
stopifnot(all(df$ride_length > 0, na.rm = TRUE))
stopifnot(all(df$ride_length < 1440, na.rm = TRUE))
stopifnot(all(!is.na(df$member_casual)))
stopifnot(all(!is.na(df$rideable_type)))

# 12) Outputs
out_csv <- file.path(out_dir, "data_clean.csv")
out_rds <- file.path(out_dir, "data_clean.rds")
write_csv(df, out_csv)
saveRDS(df, out_rds)

# 13) Quick executive summary
summary_tbl <- df |>
  summarise(
    total_rows = n(),
    start_date = min(date, na.rm = TRUE),
    end_date   = max(date, na.rm = TRUE),
    members = sum(member_casual == "member"),
    casuals = sum(member_casual == "casual"),
    pct_members = round(100 * members / n(), 1),
    median_duration_min = round(median(ride_length, na.rm = TRUE), 1),
    p95_duration_min = round(quantile(ride_length, 0.95, na.rm = TRUE), 1)
  )
print(summary_tbl)

message("Datasets saved in data/processed/. Ready for summary and EDA.")

