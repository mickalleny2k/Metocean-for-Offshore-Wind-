library(magrittr)
library(dplyr)
library(lubridate)
##Format wind file
#import file
wind <- read.csv('data/wind/wind.csv')

wind$time <- as.POSIXct(paste(wind$Time),
                      tz = "GMT",
                      format = "%d/%m/%Y %H:%M")

#wind - arrange time according to earliest to oldest
wind <- wind %>% 
  arrange(time)
#remove unwanted columns
wind <- wind[, -c(1)]
#rearrange columns
wind <- wind[, c(3, 1, 2)]

#rename columns
colnames(wind) <- c('time', 'wspeed', 'wdir')

#save dataset
write.csv(wind, "output/dataset/wind_compiled.csv", row.names = FALSE)




###############################################
#Format tide file
#import file
tide <- read.csv('data/waterlevels_currents/water_level_currents.csv')

tide$time <- as.POSIXct(paste(tide$Time),
                        tz = "GMT",
                        format = "%Y-%m-%d %H:%M:%S")

#wind - arrange time according to earliest to oldest
tide <- tide %>% 
  arrange(time)
tide <- tide[, -c(1)]
tide <- tide[, c(4, 1:3)]
colnames(tide) <- c('time', 'c_dir', 'c_speed', 'water_level')
write.csv(tide, "output/dataset/tide_compiled.csv", row.names = FALSE)
