library(ggplot2)

ggplot(waves, aes(x = Tp, y = Hs)) +
  geom_point(alpha = 0.6, color = "blue") +       # scatter points
  geom_density_2d(color = "red") +               # 2D KDE contours
  labs(x = "Peak Period Tp (s)", y = "Significant Wave Height Hs (m)",
       title = "Scatter plot of Waves with 2D Kernel Density") +
  theme_minimal()

ggplot(waves, aes(x = Tp, y = Hs)) +
  geom_point(alpha = 0.5) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.3) +
  scale_fill_viridis_c() +
  labs(x = "Peak Period Tp (s)", y = "Significant Wave Height Hs (m)",
       title = "Scatter plot of Waves with Density Shading") +
  theme_minimal()
