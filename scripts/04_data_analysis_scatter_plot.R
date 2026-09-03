#install packages
library(tidyverse)
library(magrittr)

#Bin wave height into categories
wave <- wave %>%
  mutate(height_bin = cut(hs, breaks=seq(0,16, by = 2),include.lowest= TRUE))

#Scatter diagram for incident wave conditions and kernel density
#colour palette
cols <- rev(rainbow(20)[-c(3,6,8,10,12,13,14,16,17,18,19,20)])

plot_kernel <- ggplot(wave,aes(x=tp,y=hs)) +xlab("Peak wave period (Tp) (s)") +
  ylab("Significant wave height (Hs) (m)") +
  geom_bin2d(bins=110, aes(fill=after_stat(count))) +
  labs(fill="number of occurrences") +
  scale_fill_gradientn(colours=cols, breaks=c(1,500,1000,1500,2000,2500)) +
  theme_bw() +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,16)) +
  ggtitle("Tp (s) vs Hs (m)") +
  theme_linedraw()+
  theme(plot.title=element_text(hjust=0.5), legend.text=element_text(size=10)) +
  guides(fill=guide_colorbar(barwidth =1, barheight=6))
plot_kernel

ggsave(plot_kernel, file="output/plots/hs_tp_kernel_density.jpeg", dpi=600, height=5, width=7, units=c("in"))
  
  
