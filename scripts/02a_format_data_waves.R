#load packages
library(tidyverse)
library(magrittr)
library(readr)
library(lubridate)

#Import wave timeseries
hs <- read.csv('data/wave/wave_hs.csv')
tp <- read.csv('data/wave/wave_tp.csv')
mwd <- read.csv('data/wave/wave_mwd.csv')

#Format files so they are ready to combine.

# The 'Time' column in the imported CSV file is not yet recognised as time in R. 
# Therefore, we have to create a new 'time' column using the function 'as.POISVct()' with 'time' as input. 
# As our new column is the same name as our existing column it is overwriting it.
hs$time <- as.POSIXct(paste(hs$Time),
                      tz = "GMT",
                      format = "%d/%m/%Y %H:%M")

#hs - arrange time according to earliest to oldest
hs <- hs %>% 
  arrange(time)

#Same for mwd & tp but also remove time column as this is retained in hs dataset.
tp$time <- as.POSIXct(paste(tp$Time),
                      tz = "GMT",
                      format = "%d/%m/%Y %H:%M")
tp <- tp %>% 
  arrange(time)

mwd$time <- as.POSIXct(paste(mwd$Time),
                       tz = "GMT",
                       format = "%d/%m/%Y %H:%M")
mwd <- mwd %>% 
  arrange(time)

#create a new object 'wave' and combine all columns using function 'cbind()'
wave <- cbind(hs, tp, mwd)

#remove all unwanted columns
wave <- wave[, -c(1, 4, 6, 7, 9)]
#rearrange columns
wave <- wave[, c(2, 1, 3, 4)]

#save formatted dataset
write.csv(wave, 'output/dataset/wave_compiled.csv')
