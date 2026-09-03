#install packages
library(tidyverse)
library(magrittr)

wave$month <- factor(format(wave$time, "%m"), labels = month.name)

monthly_stats <- wave %>%
  group_by(month) %>%
  summarise("mean"=mean(hs),
            "median"=median(hs),
            "sd"=sd(hs),
            "max"=max(hs),
            "min"=min(hs))

overall_stats <- wave %>%
  summarise("mean"=mean(hs),
            "median"=median(hs),
            "sd"=sd(hs),
            "max"=max(hs),
            "min"=min(hs))

overall_stats$month <- c('Overall')
overall_stats <- overall_stats[, c(6, 1:5)] 
table_hs <- rbind(monthly_stats, overall_stats)
write.csv(table_hs, "output/tables/hs_stats.csv")

  