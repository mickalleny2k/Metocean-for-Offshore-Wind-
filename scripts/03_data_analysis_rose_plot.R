library(openair)

#basic wind rose plot
png("output/plots/windrose_plot.png", width = 1500, height = 1500, res = 200)
windRose(wind, ws = "wspeed", wd="wdir", angle = 22.5, breaks = 6, ws.int=4, calm.thresh=0,
         dig.lab=1, width=1.5, key.footer= "wind speed(m/s)", annotate=FALSE, 
         main = "wind rose (10m above sea level)", 
         ylab = "frequency of count by wind direction(%)")
dev.off()

#basic wave rose plot
png("output/plots/waverose_plot.png", width = 1500, height = 1500, res = 200)
windRose(wave, ws = "hs", wd="mwd", angle = 22.5, breaks = 7, ws.int=2, calm.thresh=0,
         dig.lab=1, width=2, key.footer= "significant wave height(m)", annotate=FALSE, 
         main = " wave rose", 
         ylab = "frequency of count by mean wave direction(%)")
dev.off()



