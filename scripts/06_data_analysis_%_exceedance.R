thresholds <- seq(0, 14, by = 1)

exceedance <- sapply(thresholds, function(thresh) {
  mean(wave$hs > thresh) * 100
})

exceedance_table <- data.frame(
  Threshold = thresholds,
  Percent_exceedance = exceedance
)

write.csv(exceedance_table, "output/tables/exceedance_hs_table.csv")
