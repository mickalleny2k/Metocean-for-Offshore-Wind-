library(dplyr)

HS_LIMIT  <- 2.0      # metres
WIND_LIMIT <- 10.0    # m/s
MIN_WINDOW_HOURS <- 6 # minimum duration of a usable window

data$ok <- with(data, hs <= HS_LIMIT & Speed_10m <= WIND_LIMIT)

# Ensure ordered by time
data <- data %>% arrange(Time)

# Detect transitions between safe/unsafe
data <- data %>%
  mutate(group = cumsum(c(1, diff(ok)) != 0))

windows <- data %>%
  group_by(group) %>%
  summarise(
    ok = first(ok),
    start = first(Time),
    end   = last(Time),
    duration_hours = as.numeric(difftime(last(Time), first(Time), units = "hours"))
  ) %>%
  filter(ok == TRUE & duration_hours >= MIN_WINDOW_HOURS) %>%
  select(start, end, duration_hours)

# Save weather window table
write.csv(windows,
          "./output/tables/weather_windows.csv",
          row.names = FALSE)