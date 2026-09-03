library(TideHarmonics)

tide.der <-  ftide(tide$water_level, tide$time, hc114)

SD_tide <- tide.der$features1
SD_tide.df <- as.data.frame(SD_tide)
SD_tide.df <- as.data.frame(t(SD_tide.df))

start <- min(tide$time)
end <- start + lubridate::years(19)

predicted <- predict(tide.der, start, end, 1)
HAT <- max(predicted, na.rm = TRUE)
LAT <- max(predicted, na.rm = TRUE)

datums <- cbind(SD_tide.df,HAT, LAT)

datums <- datums[, c('HAT', 'MHWS', 'MHWN', 'MSL', 'MLWN', 'MLWS', 'LAT')]

datums_t <- as.data.frame(t(datums))

write.csv(datums_t, "output/tables/tidal_datums.csv")
