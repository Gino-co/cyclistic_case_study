# 03_eda.R — Advanced Exploratory Analysis & Exporting Outputs
# Objective: Explore usage patterns, generate metrics and professional visualizations for reporting

# 0) Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(janitor)
})

# 1) Configuration
TZ <- "America/Chicago"
in_file <- "data/processed/data_clean.csv"
out_dir <- "data/processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 2) Load cleaned data
df <- read_csv(in_file, show_col_types = FALSE)

# 3) Metrics by user type
metrics_user <- df %>%
  group_by(member_casual) %>%
  summarise(
    total_rides = n(),
    avg_duration_min = mean(ride_length),
    median_duration_min = median(ride_length),
    duration_p95_min = quantile(ride_length, 0.95),
    .groups = "drop"
  )
write_csv(metrics_user, file.path(out_dir, "metrics_user.csv"))

# 4) Average duration by day of week
metrics_dow <- df %>%
  group_by(day_of_week, member_casual) %>%
  summarise(
    avg_duration_min = mean(ride_length),
    median_duration_min = median(ride_length),
    .groups = "drop"
  )
write_csv(metrics_dow, file.path(out_dir, "metrics_by_dow.csv"))

# 5) Average duration by month
metrics_month <- df %>%
  group_by(month, member_casual) %>%
  summarise(
    avg_duration_min = mean(ride_length),
    median_duration_min = median(ride_length),
    .groups = "drop"
  )
write_csv(metrics_month, file.path(out_dir, "metrics_by_month.csv"))

# 6) Top stations
top_start <- df %>%
  count(start_station_name, sort = TRUE, name = "n") %>%
  slice_head(n = 10)
write_csv(top_start, file.path(out_dir, "top_start_stations.csv"))

top_end <- df %>%
  count(end_station_name, sort = TRUE, name = "n") %>%
  slice_head(n = 10)
write_csv(top_end, file.path(out_dir, "top_end_stations.csv"))

# 7) Professional Visualizations

# Histogram of trip durations
p_hist <- ggplot(df, aes(x = ride_length, fill = member_casual)) +
  geom_histogram(bins = 100, alpha = 0.6, position = "identity") +
  scale_x_continuous(limits = c(0, 120), labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member"="#1f77b4", "casual"="#ff7f0e")) +
  labs(title = "Trip Duration by User Type", x = "Minutes", y = "Number of Trips", fill = "User Type") +
  theme_minimal()
ggsave(file.path(out_dir, "hist_duration.png"), p_hist, width = 7, height = 4)

# Boxplot of trip durations including outliers
p_box <- ggplot(df, aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_continuous(labels = comma, limits = c(0, 1440)) +
  scale_fill_manual(values = c("member"="#1f77b4", "casual"="#ff7f0e")) +
  labs(title = "Trip Duration by User Type (Including Outliers)", x = "", y = "Minutes", fill = "User Type") +
  theme_minimal()
ggsave(file.path(out_dir, "box_duration.png"), p_box, width = 6, height = 4)

# Trips by hour
by_hour <- df %>%
  count(hour, member_casual)
p_hour <- ggplot(by_hour, aes(x = hour, y = n, color = member_casual)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("member"="#1f77b4", "casual"="#ff7f0e")) +
  labs(title = "Trips by Hour of Day", x = "Hour", y = "Number of Trips", color = "User Type") +
  theme_minimal()
ggsave(file.path(out_dir, "trips_by_hour.png"), p_hour, width = 7, height = 4)

# Trips by bike type
by_bike <- df %>%
  count(rideable_type, member_casual)
p_bike <- ggplot(by_bike, aes(x = rideable_type, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member"="#1f77b4", "casual"="#ff7f0e")) +
  labs(title = "Trips by Bike Type", x = "Bike Type", y = "Number of Trips", fill = "User Type") +
  theme_minimal()
ggsave(file.path(out_dir, "trips_by_bike.png"), p_bike, width = 6, height = 4)

message("EDA completed. Metrics and professional visualizations saved in data/processed/.")
