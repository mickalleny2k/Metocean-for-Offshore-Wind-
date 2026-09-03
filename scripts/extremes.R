###############################################################
#   COMBINED WIND + WAVE + WATER-LEVEL METOCEAN ANALYSIS
#   Lightweight script using evd::fgev (no heavy dependencies)
###############################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(evd)         # For GEV fitting
library(jsonlite)
library(tidyr)

# ======================= USER INPUTS =========================
wind_file  <- "./data/wind/wind.csv"                   # Speed_10m, dir_10m
wl_file    <- "./data/waterlevels_currents/water_level_currents.csv"   # ssh.metres.mc
hs_file    <- "./data/wave/wave_hs.csv"                # hs
tp_file    <- "./data/wave/wave_tp.csv"                # tp
mwd_file   <- "./data/wave/wave_mwd.csv"               # mwd

rho_air   <- 1.225
rho_water <- 1025
g         <- 9.80665

bins <- seq(0, 360, by = 30)
bin_mid <- bins[-1] - 15
# ============================================================


# ---------------- YEAR EXTRACTION FUNCTION ------------------
extract_year <- function(t){
  s <- as.character(t)
  if (grepl("/", s)) return(as.integer(substr(s, nchar(s)-3, nchar(s))))
  if (grepl("-", s)) return(as.integer(substr(s, 1, 4)))
  m <- regmatches(s, regexpr("[0-9]{4}", s))
  if (length(m) == 0) return(NA)
  return(as.integer(m))
}


# ============================================================
#   PART 1 — WIND ANALYSIS
# ============================================================

cat("\n=== WIND ANALYSIS ===\n")

wind <- fread(wind_file) %>% drop_na(Speed_10m)

wind$Time <- as.POSIXct(
  wind$Time,
  format = "%d/%m/%Y %H:%M",
  tz = "UTC"
)

wind$year <- as.integer(format(wind$Time, "%Y"))

wind_ann <- wind %>%
  group_by(year) %>%
  summarise(max_wind = max(Speed_10m, na.rm = TRUE))

write.csv(wind_ann, "wind_annual_max.csv", row.names = FALSE)

# ---- GEV fit ----
gev_fit <- function(series, periods = c(2,5,10,20,50,100)){
  fit <- fgev(series)
  loc <- fit$estimate["loc"]
  scale <- fit$estimate["scale"]
  shape <- fit$estimate["shape"]
  rl <- sapply(periods, function(rp){
    p <- 1 - 1/rp
    qgev(p, loc=loc, scale=scale, shape=shape)
  })
  names(rl) <- paste0(periods, "-yr")
  return(list(params = fit$estimate, RL = rl))
}

wind_gev <- gev_fit(wind_ann$max_wind)
write_json(wind_gev, "wind_return_levels.json", pretty=TRUE)

# ---- Directional energy ----
wind$bin <- cut(wind$dir_10m %% 360, bins, labels=bin_mid, include.lowest = TRUE)
wind$power <- 0.5 * rho_air * (wind$Speed_10m^3)

wind_dir <- wind %>%
  group_by(bin) %>%
  summarise(mean_power = mean(power),
            total_GWh_m2 = sum(power) * 1e-9)

write.csv(wind_dir, "wind_directional_energy.csv", row.names=FALSE)

ggplot(wind_dir, aes(x = as.numeric(as.character(bin)), y = mean_power)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("Wind direction (°)") +
  ylab("Mean wind power (W/m²)") +
  ggtitle("Directional Wind Energy")
ggsave("wind_directional_energy_rose.png", dpi=150)


# ============================================================
#   PART 2 — WATER LEVEL ANALYSIS
# ============================================================

cat("\n=== WATER LEVEL ANALYSIS ===\n")

wl <- fread(wl_file)
wl$year <- sapply(wl$Time, extract_year)

wl_ann <- wl %>%
  group_by(year) %>%
  summarise(max_ssh = max(ssh.metres.mc, na.rm = TRUE))

write.csv(wl_ann, "waterlevel_annual_max.csv", row.names=FALSE)

# 1️⃣ Define gev_fit BEFORE calling it
gev_fit <- function(series, periods = c(2,5,10,20,50,100)) {
  series <- series[!is.na(series)]          # remove missing values
  fit <- fgev(series, std.err = FALSE)      # avoid singular information errors
  
  loc <- fit$estimate["loc"]
  scale <- fit$estimate["scale"]
  shape <- fit$estimate["shape"]
  
  rl <- sapply(periods, function(rp){
    p <- 1 - 1/rp
    qgev(p, loc = loc, scale = scale, shape = shape)
  })
  names(rl) <- paste0(periods, "-yr")
  
  return(list(params = fit$estimate, RL = rl))
}

# 2️⃣ Clean data and call gev_fit
wl_data <- wl_ann$max_ssh
wl_gev <- gev_fit(wl_data)

# 3️⃣ Save results
write_json(wl_gev, "waterlevel_return_levels.json", pretty=TRUE)

# ============================================================
#   PART 3 — WAVE ANALYSIS (Hs, Tp, MWD)
# ============================================================

cat("\n=== WAVE ANALYSIS ===\n")

hs <- fread(hs_file)
tp <- fread(tp_file)
mwd <- fread(mwd_file)

# ---- Merge wave files ----
waves <- hs %>%
  left_join(tp,  by="Time") %>%
  left_join(mwd, by="Time") %>%
  arrange(Time)

waves$Time <- as.POSIXct(
  waves$Time,
  format = "%d/%m/%Y %H:%M",
  tz = "UTC"
)

waves$year <- as.integer(format(waves$Time, "%Y"))

#waves$year <- sapply(waves$Time, extract_year)

# ---- Annual maxima of Hs ----
wave_ann <- waves %>%
  group_by(year) %>%
  summarise(max_hs = max(hs, na.rm = TRUE))

write.csv(wave_ann, "wave_Hs_annual_max.csv", row.names=FALSE)

wave_data <- wave_ann$max_hs
wave_data <- wave_data[!is.na(wave_data)]

wave_gev <- gev_fit(wave_data)
write_json(wave_gev, "wave_Hs_return_levels.json", pretty=TRUE)

# ---- Directional wave energy ----
waves_clean <- waves %>% drop_na(hs, mwd)
waves_clean$bin <- cut(waves_clean$mwd %% 360, bins, labels=bin_mid, include.lowest = TRUE)

waves_clean$E <- 0.125 * rho_water * g * (waves_clean$hs^2)

wave_dir <- waves_clean %>%
  group_by(bin) %>%
  summarise(mean_J = mean(E),
            total_J = sum(E),
            GWh_m2 = total_J * 2.77778e-13)

write.csv(wave_dir, "wave_directional_energy.csv", row.names=FALSE)

ggplot(wave_dir, aes(x = as.numeric(as.character(bin)), y = GWh_m2)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("Wave direction (°)") +
  ylab("Total wave energy (GWh/m²)") +
  ggtitle("Directional Wave Energy")
ggsave("wave_directional_energy_rose.png", dpi=150)


# ============================================================
#   REPORT SUMMARY (printed to console)
# ============================================================

cat("\n\n=====================================\n")
cat("       METOCEAN ANALYSIS COMPLETE\n")
cat("=====================================\n\n")

cat("Generated files:\n")
cat(" • wind_annual_max.csv\n")
cat(" • wind_return_levels.json\n")
cat(" • wind_directional_energy.csv / .png\n\n")

cat(" • waterlevel_annual_max.csv\n")
cat(" • waterlevel_return_levels.json\n\n")

cat(" • wave_Hs_annual_max.csv\n")
cat(" • wave_Hs_return_levels.json\n")
cat(" • wave_directional_energy.csv / .png\n\n")

cat("All outputs saved in working directory.\n")
