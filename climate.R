require(inmet)

stations <- station_info %>%
  mutate(Latitude = latitude, Longitude = longitude) 
coordinates(stations) = ~Longitude+Latitude
stations <- st_as_sf(stations, crs = 4326)
st_crs(stations) <- 4326

ecoregions_stations <- lapply(st_intersects(stations, ecoregions_br), function(x) ifelse(length(x) == 1, x, NA))
stations$ecoregion <- as.character(ecoregions_br$ECO_NAME[unlist(ecoregions_stations)])

stations_selected_eco <- filter(stations, ecoregion %in% selected_ecoregions$ecoregion)

stations_data <- station_data_summary %>%
  left_join(select(stations_selected_eco, station_id, ecoregion))

temp <- stations_data %>%
  filter(!is.na(ecoregion)) %>%
  mutate(ecoregion = gsub("Campos Rupestres Montane Savanna", "Campos Rupestres", ecoregion)) %>%
  group_by(ecoregion, month = lubridate::month(date, label = TRUE)) %>%
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE))

temp_trend <- stations_data %>%
  filter(!is.na(ecoregion)) %>%
  mutate(ecoregion = gsub("Campos Rupestres Montane Savanna", "Campos Rupestres", ecoregion)) %>%
  group_by(ecoregion, year = lubridate::year(date)) %>%
  summarise(temp_mean = mean(temp_mean, na.rm = TRUE)) 

by_eco_climate <- temp_trend %>%
  group_by(ecoregion) %>%
  nest()

temp_trend_model <- function(df) {
  lm(temp_mean ~ year, data = df)
}

by_eco_climate <- by_eco_climate %>% 
  mutate(model = map(data, temp_trend_model))

glance_eco <- by_eco_climate %>%
  mutate(temp_trend = map(model, function(x) coefficients(x)[2])) %>% 
  unnest(temp_trend, .drop = TRUE)

glance_eco$ecoregion <- gsub("Campos Rupestres", "Campos Rupestres Montane Savanna", glance_eco$ecoregion)

p_temp_trend <- temp_trend %>%
  ggplot(aes(x = year, y = temp_mean, group = 1)) +
  geom_line() + 
  geom_smooth(method = "lm") +
  theme_bw() +
  ylab("Mean annual temperature (ºC)") +
  xlab("Year") + 
  facet_wrap(~ecoregion, ncol = 8)

p_temp <- temp %>%
  ggplot() + 
  geom_line(aes(x = month, y = temp_mean, group = 1)) + 
  ylab("Mean temperature (ºC)") +
  xlab("Month") + 
  facet_wrap(~ecoregion, ncol = 8) + 
  scale_x_discrete(breaks = month.abb[c(1, 3, 5, 7, 9, 11)]) + 
  theme_bw()

rainfall <- stations_data %>%
  filter(!is.na(ecoregion)) %>%
  mutate(ecoregion = gsub("Campos Rupestres Montane Savanna", "Campos Rupestres", ecoregion)) %>%
  group_by(station_id, month = lubridate::month(date, label = TRUE), year = lubridate::year(date), ecoregion) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ecoregion, month) %>%
  summarise(prec = mean(prec, na.rm = TRUE))

rainfall_trend <- stations_data %>%
  filter(!is.na(ecoregion)) %>%
  mutate(ecoregion = gsub("Campos Rupestres Montane Savanna", "Campos Rupestres", ecoregion)) %>%
  group_by(year = lubridate::year(date), ecoregion, station_id) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year, ecoregion) %>%
  summarise(prec = mean(prec, na.rm = TRUE))


p_rainfall_trend <- rainfall_trend %>%
  ggplot(aes(x = year, y = prec, group = 1)) + 
  geom_line() + 
  geom_smooth(method = "lm") + 
  ylab("Mean total rainfall (mm)") +
  xlab(NULL) + 
  facet_wrap(~ecoregion, ncol = 8) + theme_bw()

p_fig02_ms03 <- grid.arrange(p_rainfall_trend, p_temp_trend, ncol = 1)
ggsave(p_fig02_ms03, file = "p_fig02_ms03.png", width = 15, height = 4)

p_rainfall <-  rainfall %>%
  ggplot() + 
  geom_line(aes(x = month, y = prec, group = 1)) + 
  xlab(NULL) + 
  ylab("Mean total rainfall (mm)") +
  facet_wrap(~ecoregion, ncol = 8) + 
  scale_x_discrete(breaks = month.abb[c(1, 3, 5, 7, 9, 11)]) + 
  theme_bw()

p_fig02 <- grid.arrange(p_rainfall, p_temp, ncol = 1)
ggsave(p_both, file = "p_fig02.png", width = 15, height = 4)
  
# stations plot
p_stations <-
ggplot() + 
  geom_sf(data = br, fill = "white", size = 0.2, colour = "black") + 
  geom_sf(data = filter(ecoregions_br, ECO_NAME %in% selected_ecoregions$ecoregion), aes(fill = ECO_NAME), size = 0.2, colour = "black") + 
  geom_point(data = filter(stations, ecoregion %in% selected_ecoregions$ecoregion), aes(x = longitude, y = latitude)) + 
  geom_label()
  theme_bw() + 
  xlab(NULL) + ylab(NULL) +
  scale_fill_discrete(name = "Ecoregion")

ggsave(p_stations, file = "p_stations.png", width = 12, height = 8)

p_stations2 <-
  ggplot() + 
  geom_sf(data = br, fill = "white", size = 0.2, colour = "black", alpha = 0.5) + 
  geom_sf(data = filter(ecoregions_br, ECO_NAME %in% unique(spp_selected$ecoregion)), aes(fill = ECO_NAME), size = 0.2, colour = "black", alpha = 0.5) + 
  geom_point(data = spp_selected, aes(x = longitude.x, y = latitude.x), size = 0.5, alpha = 0.8) + 
  geom_point(data = stations %>% filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")), aes(x = longitude, y = latitude), col = "red", size = 2) + 
  geom_label_repel(data = stations %>% filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")), aes(x = longitude, y = latitude), label = c("Brasilia", "Catalão", "Curitiba", "São Paulo"), alpha = 0.8, point.padding = 1) + 
  theme_bw() + 
  xlab(NULL) + ylab(NULL) + 
  scale_fill_discrete(name = "Ecoregion")

ggsave(p_stations2, file = "p_stations2.png", width = 12, height = 8)

to_doy <- function(x) {
  floor(as.numeric(x) * 365/360)
}

one_year <- seq(ymd("2017/01/01"), ymd("2017/12/31"), by = "1 day")
one_year <- data.frame(date = one_year, doy = yday(one_year))
one_year <- one_year %>%
  mutate(month = lubridate::month(date, label = TRUE))

splink_eco_stats <- 
  filter(splink_eco, ecoregion %in% selected_ecoregions$ecoregion) %>%
  filter(n >= 20) %>%
  group_by(search_str) %>%
  count()

splink_eco_summarised <- filter(splink_eco, ecoregion %in% selected_ecoregions$ecoregion) %>%
  mutate(ecoregion = gsub("Campos Rupestres Montane Savanna", "Campos Rupestres", ecoregion)) %>%
  filter(n >= 20) %>%
  group_by(ecoregion, search_str) %>%
  summarise(mean = mean.circular(circular, na.rm = TRUE), rho = rho.circular(circular, na.rm = TRUE)) %>%
  filter(rho >= 0.15) %>%
  ungroup()

splink_eco_summarised <- splink_eco_summarised %>%
  mutate(doy = to_doy(mean)) 

splink_eco_summarised <- splink_eco_summarised %>%
  left_join(select(one_year, month, doy))

splink_counts <- splink_eco_summarised %>%
  group_by(ecoregion, month) %>%
  count() %>%
  ungroup()
  
splink_counts <- splink_counts %>%
  left_join(rainfall) %>%
  left_join(temp)

p_means <- splink_eco_summarised %>%
  ggplot(aes(x = mean)) + 
  geom_histogram(bins = 20) + 
  xlab("Circular means (˚)") + 
  ylab("Number of species") +
  facet_wrap(~ecoregion, ncol = 1, scales = "free_y") + 
  theme_bw()

p_rhos <- splink_eco_summarised %>%
  ggplot(aes(x = rho)) + 
  geom_histogram(bins = 20) + 
  xlab("Mean vector length") + 
  ylab(NULL) +
  facet_wrap(~ecoregion, ncol = 1, scales = "free_y") + 
  theme_bw()

ggsave(grid.arrange(p_means, p_rhos, ncol = 2), file = "p_fig03.png", width = 5, height = 8)

p_gam_temp <- splink_counts %>%
  ungroup() %>%
  ggplot(aes(x = temp_mean, y = n)) + 
  geom_smooth(span = 2) + 
  xlab("Mean temperature (ºC)") +
  ylab(NULL) + 
  geom_point() + 
  facet_wrap(~ecoregion, scales = "free", ncol = 1) + 
  theme_bw()

p_gam_rainfall <- splink_counts %>%
  ungroup() %>%
  ggplot(aes(x = prec, y = n)) + 
  geom_smooth(span = 2) + 
  xlab("Monthly rainfall (mm)") +
  ylab("Number of spp") + 
  geom_point() + 
  facet_wrap(~ecoregion, ncol = 1, scales = "free") +
  theme_bw()

ggsave(grid.arrange(p_gam_rainfall, p_gam_temp, ncol = 2), file = "p_fig04.png", width = 5, height = 11)

spp_ms02 <- splink_eco_summarised %>%
  filter(rho >= 0.6) %>%
  dplyr::select(ecoregion, search_str) %>%
  right_join(splink_eco) %>%
  mutate(dl = daylength(latitude, doy))

p_dl <- spp_ms02 %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  ggplot(aes(x = dl)) + 
  geom_density() + 
  facet_wrap(~ecoregion, scales = "free_y", ncol = 4) + 
  theme_bw() + 
  xlab("Day length") + 
  ylab("Density")

ggsave(p_dl, file = "p_dl.png", width = 10, height = 5)
