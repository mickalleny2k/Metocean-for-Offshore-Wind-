library(dplyr)
library(lubridate)
library(zoo)

data <- cbind(wind, wave)
data <- data[,c(1,2,5)]

data$calendar_month <- format(data$time, "%B")

wave_thresholds <- c(1.5, 2, 2.5)
wind_thresholds <- c(10,15)
durations <- c(3, 6)

dt_hours <-as.numeric(difftime(data$time[2], data$time[1], units = "hours"))
window_sizes <- durations / dt_hours

results <- list()

for (wthr in wave_thresholds) {
  for (windthr in wind_thresholds) {
    for (wsize in window_sizes) {
      
      duration_hours <- wsize * dt_hours
      label <- paste0("wave<", wthr, "m & wind<", windthr, "m/s for", duration_hours, "h")
      
      data <- data %>%
        mutate(
          pass = (hs < wthr) & (wspeed < windthr),
          pass_roll = rollapply(pass, width = wsize, FUN = all, align = "left", fill = NA),
          valid_window = ifelse(pass_roll, 1, 0)
        )
      
      monthly_summary <- data %>%
        group_by(calendar_month) %>%
        summarise(
          total_periods = n(),
          valid_windows = sum(valid_window, na.rm = TRUE),
          percent_valid = 100 * valid_windows / total_periods
        ) %>%
        mutate(Condition = label)
      results[[label]] <- monthly_summary
    }
  }
}