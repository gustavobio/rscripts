require(tidyverse)
require(fuzzyjoin)
require(inmet)
require(lubridate)

mapa_caatinga <- st_read("caatinga/")

st_crs(mapa_caatinga) <- 4326
within_caatinga <- st_intersects(st_geometry(mapa_caatinga), splink)
splink_caatinga <- splink[unlist(within_caatinga), ]

p_caatinga <- splink_caatinga %>%
  ggplot() +
  geom_sf(data = br, fill = "white", size = 0.1, colour = "black") + 
  geom_sf(data = mapa_caatinga, fill = "white", size = 0.1, colour = "lightgrey", alpha = 0.1) + 
  geom_point(aes(x = longitude, y = latitude), colour = "darkblue", size = 0.3, alpha = 0.7) +
  labs(x = NULL, y = NULL) + 
  theme_minimal()

p_caatinga2 <-
  ggplot() +
  geom_sf(data = mapa_caatinga, fill = "white", size = 0.1, colour = "lightgrey", alpha = 0.1) + 
  labs(x = NULL, y = NULL) + 
  theme_minimal()

ggsave(p_caatinga, file = "p_caatinga.png", width = 12, height = 12)
ggsave(p_caatinga2, file = "p_caatinga2.png", width = 12, height = 12)

splink_caatinga <- splink_caatinga %>%
  mutate(doy_april = ifelse(doy > 92, doy - 92, doy + 365-92)) %>%
  mutate(doy_june = ifelse(doy > 183, doy - 183, doy + 365 - 183)) %>%
  mutate(doy_sep = ifelse(doy > 273, doy - 273, doy + 365 - 273))

top_var_caatinga <- as.data.frame(splink_caatinga) %>%
  mutate(doy_circ = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "2pi", type = "angles", template = "none", zero = 0)) %>%
  group_by(scientificname) %>%
  summarise(var_circ = var.circular(doy_circ)) %>%
  ungroup() %>%
  arrange(var_circ)

top_caatinga_spp <- as.data.frame(splink_caatinga) %>%
  count(scientificname, sort = TRUE) %>%
  left_join(top_var_caatinga)

selected <- filter(top_caatinga_spp, n >= 100 & var_circ <= 0.4)

p_caatinga_year <- splink_caatinga %>%
  filter(scientificname %in% selected$scientificname) %>%
  filter(yearcollected >= 1970) %>%
  filter(between(doy, quantile(doy, na.rm = TRUE, probs = c(0.05, 0.95))[1], quantile(doy, na.rm = TRUE,probs = c(0.05, 0.95))[2])) %>%
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm") +
  ylim(c(0,366)) + 
  theme_minimal() + 
  facet_wrap(~scientificname, ncol = 5) + 
  xlab("Year collected") + 
  ylab("DOY")

ggsave(p_caatinga_year, file = "p_caatinga_year.pdf", width = 12, height = 8)

splink_caatinga %>%
  filter(scientificname %in% selected$scientificname) %>%
  filter(yearcollected >= 1970) %>%
  filter(between(doy, quantile(doy, na.rm = TRUE, probs = c(0.05, 0.95))[1], quantile(doy, na.rm = TRUE,probs = c(0.05, 0.95))[2])) %>%
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm") +
  ylim(c(0,366)) + 
  theme_minimal() + 
  facet_wrap(~scientificname, ncol = 5)

require(circular)

first_doy <- function(doy) {
  sds <- sapply(1:365, function(x) {
    days <- ifelse(doy > x, doy - x, doy + 365 - x)
    days_circ <- as.circular(days * (360/366), units = "degrees", rotation = "clock", modulo = "2pi")
    var.circular(days_circ)
  })
  first_day <- which.min(sds)
  ifelse(doy > first_day, doy - first_day, doy + 365 - first_day)
}

test_spp <- splink_caatinga %>%
  filter(scientificname == "Aspidosperma pyrifolium") %>%
  select(doy)
test_spp <- test_spp$doy

splink_caatinga <- as.data.frame(splink_caatinga)
splink_caatinga$n_id <- 1:nrow(splink_caatinga)
test_caatinga <- geo_left_join(splink_caatinga, station_info, by = c("latitude", "longitude"), max_dist = 50, unit = "km", distance_col = "distance")

test_caatinga <- test_caatinga %>%
  group_by(n_id) %>%
  dplyr::slice(which.min(distance)[1]) %>%
  ungroup()

test_caatinga <- test_caatinga %>%
  mutate(Data = ymd(paste(yearcollected, monthcollected, daycollected, sep = "/")))

test_caatinga2 <- left_join(test_caatinga, station_data_summary, by = c("station_id" = "station_id", "Data" = "date"))

p_caatinga_temp <- test_caatinga2 %>%
  filter(scientificname %in% selected$scientificname) %>%
  filter(!is.na(temp_mean)) %>%
  filter(yearcollected >= 1970) %>%
  filter(between(doy, quantile(doy, na.rm = TRUE, probs = c(0.05, 0.95))[1], quantile(doy, na.rm = TRUE,probs = c(0.05, 0.95))[2])) %>%
  ggplot(aes(x = temp_mean, y = doy)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm") +
  ylim(c(0,366)) + 
  theme_minimal() + 
  facet_wrap(~scientificname, ncol = 5) + 
  xlab("Mean temperature (ºC)") + 
  ylab("DOY")

ggsave(p_caatinga_temp, file = "p_caatinga_temp.pdf", width = 12, height = 8)
