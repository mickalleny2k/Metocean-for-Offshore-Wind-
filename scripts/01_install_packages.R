# First we are creating an object with a list of the packages that we'll need

list.of.packages <- c('lubridate', 'dplyr', 'magrittr', 'tidyverse', 
                      'openair', 'ggplot2', 'readr', 'zoo', 'TideHarmonics') 

# Now we will check to see if any of the packages required are not yet on our system

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Any package missing will be added to the ‘new.packages’ object 
# which can then be used to install any missing ones

if(length(new.packages)) install.packages(new.packages)

install.packages('openair')

