library(ggplot2)

waves <- read.csv("./data/wave/wave_hs.csv")
wind  <- read.csv("./data/wind/wind.csv")

waves$Time <- as.POSIXct(waves$Time, format="%d/%m/%Y %H:%M")
wind$Time  <- as.POSIXct(wind$Time,  format="%d/%m/%Y %H:%M")

data <- merge(waves, wind, by="Time")
head(data)

p <- ggplot(data, aes(x = Speed_10m, y = hs)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(
    x = "Wind Speed at 10m (m/s)",
    y = "Significant Wave Height Hs (m)",
    title = "Hs vs Wind Speed"
  )

ggsave("./output/plots/hs_wind_density.png",
       plot = p,
       width = 8,
       height = 6,
       dpi = 300)