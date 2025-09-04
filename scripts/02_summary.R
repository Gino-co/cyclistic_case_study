# 02_summary.R — Executive summary and key metrics
# Objective: generate summaries and checks from cleaned data for presentation

# 0) Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(scales)
})

# 1) Configuration
TZ <- "America/Chicago"
in_file  <- "data/processed/data_clean.csv"
out_dir  <- "data/processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 2) Load cleaned data
df <- read_csv(in_file, show_col_types = FALSE)
message("✅ Data loaded successfully.")
print(glimpse(df, width = 80))

# 3) General checks
n_total <- nrow(df)
n_missing <- sum(!complete.cases(df))
date_range <- range(df$started_at, na.rm = TRUE)

message("Summary of dataset:")
cat("Total rows:", n_total, "\n")
cat("Rows with NA:", n_missing, "\n")
cat("Date range:", format(date_range[1]), "to", format(date_range[2]), "\n")

# 4) Key metrics
summary_metrics <- df %>%
  summarise(
    total_rides = n(),
    avg_duration_min = mean(ride_length, na.rm = TRUE),
    median_duration_min = median(ride_length, na.rm = TRUE),
    duration_p95_min = quantile(ride_length, 0.95, na.rm = TRUE)
  ) %>%
  rename(
    "Total Rides" = total_rides,
    "Average Duration (min)" = avg_duration_min,
    "Median Duration (min)" = median_duration_min,
    "95th Percentile Duration (min)" = duration_p95_min
  )

# 5) Rides by user type
by_user <- df %>%
  count(member_casual) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )

# 6) Rides by day of week
by_dow <- df %>%
  count(day_of_week, member_casual) %>%
  group_by(day_of_week) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Ensure days are in natural order
by_dow$day_of_week <- factor(by_dow$day_of_week, 
                             levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# 7) Rides by hour
by_hour <- df %>%
  count(hour, member_casual)

# 8) Top 10 start and end stations
top_start <- df %>%
  count(start_station_name, sort = TRUE, name = "n") %>%
  slice_head(n = 10)

top_end <- df %>%
  count(end_station_name, sort = TRUE, name = "n") %>%
  slice_head(n = 10)

# 9) Export CSVs
write_csv(summary_metrics, file.path(out_dir, "summary_metrics.csv"))
write_csv(by_user,        file.path(out_dir, "summary_by_user.csv"))
write_csv(by_dow,         file.path(out_dir, "summary_by_dow.csv"))
write_csv(by_hour,        file.path(out_dir, "summary_by_hour.csv"))
write_csv(top_start,      file.path(out_dir, "top_start_stations.csv"))
write_csv(top_end,        file.path(out_dir, "top_end_stations.csv"))

# 10) Plots with professional styling
colors_user <- c("member" = "#1f78b4", "casual" = "#33a02c")

# User distribution
p1 <- ggplot(by_user, aes(x = member_casual, y = pct, fill = member_casual)) +
  geom_col() +
  scale_fill_manual(values = colors_user) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Rides by User Type", y = "Percentage (%)", x = "") +
  theme_minimal(base_size = 14)
ggsave(file.path(out_dir, "plot_user_distribution.png"), p1, width = 6, height = 4)

# Rides by day of week
p2 <- ggplot(by_dow, aes(x = day_of_week, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors_user) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Rides by Day of Week", y = "Number of Rides", x = "") +
  theme_minimal(base_size = 14)
ggsave(file.path(out_dir, "plot_by_dow.png"), p2, width = 7, height = 4)

# Rides by hour
p3 <- ggplot(by_hour, aes(x = hour, y = n, color = member_casual)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = colors_user) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Rides by Hour of Day", y = "Number of Rides", x = "Hour") +
  theme_minimal(base_size = 14)
ggsave(file.path(out_dir, "plot_by_hour.png"), p3, width = 7, height = 4)

message("✅ Summary metrics and charts exported to 'data/processed/'")

