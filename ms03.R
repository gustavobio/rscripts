require(inmet)
require(fuzzyjoin)
require(tidyverse)
require(stringr)
require(lubridate)
require(ggridges)

climate_summary <- station_data_summary %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(station_id, month, year) %>%
  summarise(
    na_temp_max = sum(is.infinite(temp_max)),
    na_temp_mean = sum(is.nan(temp_mean)),
    na_temp_min = sum(is.infinite(temp_min)),
    na_insol = sum(is.infinite(insol)),
    prec = sum(prec, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_min = mean(temp_min, na.rm = TRUE),
    insol = mean(insol, na.rm = TRUE)
  ) %>%
  arrange(station_id, year, month)

climate_summary <- climate_summary %>%
  left_join(station_info) %>%
  left_join(
    climate_summary %>%
      group_by(station_id, month) %>%
      summarise(
        hist_prec = sum(prec[year <= 1990], na.rm = TRUE),
        hist_temp_max = mean(temp_max[year <= 1990], na.rm = TRUE),
        hist_temp_mean = mean(temp_mean[year <= 1990], na.rm = TRUE),
        hist_temp_min = mean(temp_min[year <= 1990], na.rm = TRUE)
      )
  )

splink_geo <- geo_left_join(splink, filter(station_info, days_missing <= 1000, year(start) <= 1971), max_dist = 100, by = c("latitude", "longitude"), unit = "km", distance_col = "distance")

splink_geo <- filter(splink_geo, !is.na(station_id))

splink_geo <- splink_geo %>%
  left_join(climate_summary, by = c("monthcollected" = "month", "yearcollected" = "year", "station_id" = "station_id"))

splink_geo <- splink_geo %>%
  group_by(id) %>%
  filter(!is.na(temp_mean), !is.na(prec)) %>%
  ungroup()

splink_geo <- splink_geo %>%
  filter(taxon_rank == "species")

splink_geo <- splink_geo %>%
  mutate(diff_temp_mean = temp_mean - hist_temp_mean) %>%
  mutate(diff_prec = prec - hist_prec)

splink_geo <- splink_geo %>%
  left_join(
    station_data_summary %>%
      mutate(doy = yday(date)) %>%
      dplyr::select(station_id, doy) %>%
      distinct()
  )

splink_geo <- splink_geo %>%
  dplyr::select(-id, -season) %>%
  distinct()

top_inst <- splink_geo %>%
  group_by(institutioncode) %>%
  count(sort = T) %>%
  ungroup() %>%
  filter(n >= 1000)

top_spp <- splink_geo %>%
  group_by(search_str) %>%
  count(sort = T) %>%
  ungroup() %>%
  filter(n >= 100)

splink_geo %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  facet_wrap(~ecoregion)

climate_summary %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  group_by(station_id) %>%
  
p_trends <- splink_geo %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  ylab("Temperature difference from historical mean (ºC)") + 
  xlab("Year") + 
  facet_wrap(~ecoregion, ncol = 4)

###
splink_geo %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean, col = ecoregion)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  ylab("Temperature difference from historical mean (ºC)") + 
  xlab("Year") + 
  theme_bw()

splink_geo %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(ecoregion %in% "Cerrado") %>%
  lm(diff_temp_mean ~ yearcollected, data = .) %>%
  summary()

ggsave(p_trends, file = "p_trends.png", width = 10, height = 5)

p_ecoregion_means <- splink_geo %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  mutate(doy = doy_break(doy, 180)) %>%
  mutate(circular = to_circular(doy)) %>%
  mutate(year = cut_width(yearcollected, 5, boundary = 1960)) %>%
  mutate(year = factor(year, labels = seq(1965, 2020, by = 5))) %>%
  group_by(ecoregion, year) %>%
  summarise(mean_circ = mean(circular, na.rm = T)) %>%
  ggplot(aes(x = year, y = mean_circ)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ylim(c(0, 360)) + 
  xlab("Year") + 
  ylab("Circular mean") + 
  facet_wrap(~ecoregion, ncol = 4) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

ggsave(p_ecoregion_means, file = "p_ecoregion_means.png", width = 10, height = 5)

### CONTINUAR AQUI

p_ecoregion_hist_means <- spp_selected %>%
  filter(search_str %in% top_spp$search_str) %>%
#  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  mutate(circular = to_circular(doy)) %>%
  left_join(splink_geo %>%
              filter(yearcollected <= 1980) %>%
              filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
              group_by(ecoregion, search_str) %>%
              summarise(hist_mean = mean(circular, na.rm = T), rho = rho.circular(circular, na.rm = T), n = n())) %>%
  filter(n >= 10) %>%
  group_by(search_str, ecoregion) %>%
  mutate(diff_circular = ang_diff(circular, hist_mean[1])) %>%
  ungroup() %>%
  filter(rho >= 0.4) %>%
  ggplot(aes(x = yearcollected, y = diff_circular)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth() + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  xlab("Year") + 
  ylab("Difference from historical means (degrees)") + 
  facet_wrap(~ecoregion, ncol = 4) + 
  theme_bw() 

ggsave(p_ecoregion_hist_means, file = "p_ecoregion_hist_means.png", width = 10, height = 5)
  
spp_life_form <- unique(filter(splink_geo, ecoregion %in% selected_ecoregions$ecoregion)$search_str)
spp_life_form <- get.taxa(spp_life_form, life.form = TRUE, habitat = T, establishment = T)

splink_geo <- splink_geo %>%
  left_join(
    dplyr::select(spp_life_form, search.str, life.form, establishment), by = c("search_str" = "search.str")
  )
  
splink_geo %>%
  filter(life.form %in% c("Herb", "Shrub", "Subshrub", "Tree", "Liana")) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean, group = life.form)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", aes(col = life.form, fill = life.form)) + 
  theme_bw() + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  ylab("Temperature difference from historical mean (ºC)") + 
  xlab("Year") + 
  facet_wrap(~ecoregion, ncol = 4)

eco_counts <- splink_geo %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  group_by(ecoregion, search_str) %>%
  count(sort = TRUE) 

by_ecoregions <- splink_geo %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  left_join(eco_counts) %>%
  filter(n >= 30) %>%
  group_by(ecoregion, search_str) %>%
  nest()

temp_model <- function(df) {
  lm(diff_temp_mean ~ yearcollected, data = df)
}

by_ecoregions <- by_ecoregions %>% 
  mutate(model = map(data, temp_model))

glance <- by_ecoregions %>% 
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  mutate(glance = map(model, function(x) coefficients(x)[2])) %>% 
  unnest(glance, .drop = TRUE)

p_glance <- glance %>%
  left_join(glance_eco) %>%
  ggplot(aes(x = glance)) + 
  geom_density(fill = "grey") + 
  xlab("Rate of change (ºC/year)") + 
  ylab("Density") + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = temp_trend), linetype = "dashed", data = glance_eco) +
  facet_wrap(~ecoregion, ncol = 4) + 
  theme_bw()

ggsave(p_glance, file = "ms03_fig05.png", width = 10, height = 4)

t_test <- function(x) {
  t.test(x$glance, mu = x$temp_trend[1])
}

glance_t <- glance %>%
  left_join(glance_eco) %>% 
  group_by(ecoregion) %>%
  nest() %>%
  mutate(t_test = map(data, t_test))

glance_t <- glance_t %>%
  mutate(glance = map(t_test, broom::glance)) %>%
  unnest(glance, .drop = T) %>%
  mutate(p.value = round(p.value, 4))

spp <- splink_geo %>%
  filter(search_str %in% top_100$search_str) %>%
  filter(na_temp_mean <= 20) %>%
 # mutate(doy = doy_break(doy, 100)) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(search_str) %>%
  mutate(temp_cut = cut_width(temp_mean, 1.5)) %>%
  mutate(years = cut_width(yearcollected, 3)) %>%
  group_by(temp_cut, search_str) %>%
  summarise(mean_doy = mean(circular), n = n()) %>%
  ungroup() %>%
  left_join(top_100) %>%
  group_by(search_str) %>%
  mutate(doy_new = doy_break(mean_doy, breakpoint = doy_start[1])) %>%
  ungroup() %>%
  filter(n > 5)

spp_years <- splink_geo %>%
  filter(search_str %in% top_100$search_str) %>%
  filter(na_temp_mean <= 20) %>%
  # mutate(doy = doy_break(doy, 100)) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(search_str) %>%
  mutate(temp_cut = cut_width(temp_mean, 1.5)) %>%
  mutate(years = cut_width(yearcollected, 3)) %>%
  group_by(years, search_str) %>%
  summarise(mean_doy = mean(circular), n = n()) %>%
  ungroup() %>%
  left_join(top_100) %>%
  group_by(search_str) %>%
 # mutate(doy_new = doy_break(mean_doy, breakpoint = doy_start[1])) %>%
  ungroup() %>%
  filter(n > 5)

spp_geo <- splink_geo %>%
  group_by(station_id, yearcollected, search_str) %>%
  count(sort = T) %>%
  filter(n >= 5)
  # mutate(doy = doy_break(doy, 100)) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(lat_range, search_str) %>%
  summarise(mean_doy = mean(circular), n = n(), rho = rho.circular(circular)) %>%
  ungroup() %>%
  filter(n > 20)

spp_geo %>%
  ggplot(aes(x = mean_doy)) +
  geom_histogram() + 
  theme_bw() + 
  facet_wrap(~lat_range)

spp %>%
  ggplot(aes(x = temp_cut, y = doy_new, group = 1)) +
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  geom_point(size = 0.2) + 
  theme_bw() +
  facet_wrap(~search_str, ncol = 10, scales = "free_x") +
  ylim(c(0, 360))

spp_years %>%
  ggplot(aes(x = years, y = mean_doy, group = 1)) +
  geom_smooth() + 
  geom_smooth(method = "lm") + 
  geom_point(size = 0.2) + 
  theme_bw() +
  facet_wrap(~search_str, ncol = 10, scales = "free_x") +
  ylim(c(0, 360))

teste <- spp %>%
  filter(search_str == "Cissus erosa")

splink_geo %>%
  filter(search_str %in% selected$search_str) %>%
  filter(na_temp_mean <= 20) %>%
  #mutate(doy = doy_break(doy, 100)) %>%
  ggplot(aes(x = doy, y = temp_mean)) +
  geom_point(alpha = 0.2) + 
  theme_bw() + 
  coord_polar() + 
  facet_wrap(~search_str, ncol = 10)


myrcia <- splink_geo %>%
  filter(search_str == "Myrcia guianensis") %>%
  filter(na_temp_mean <= 20)
  

doy_break <- function(doy, breakpoint = 100) {
  ifelse(doy > breakpoint, doy - breakpoint, doy + (365 - breakpoint))
}

selected <- splink_geo %>%
  group_by(search_str) %>%
  summarise(stations = length(unique(station_id)), lat = sd(latitude)) %>%
  arrange(desc(stations), lat) %>%
  top_n(100)

top_100 <- sort(unique(spp$search_str))[-c(7, 10, 14, 17, 19, 24, 30, 35, 37, 39, 41, 42, 43, 44, 45, 50, 51, 52, 54, 55, 56, 69, 71, 72, 79, 80, 76, 75, 74, 81, 83, 87, 89, 91, 92, 96, 99, 10)]
top_100_start <- c(rep(0, 16), 200, 200, 300, 0, 200, 0, 250, rep(0, 7), 200, 250, 0, 0, 300, 200, 0, 0, 0, 200, 0, 0, 0, 200, 0, 0, 300, 0, 200, 0, 0, 300, 0, 0, 300, 200, 220, 200, 300, 250, 0, 300, 0)

top_100 <- data.frame(search_str = top_100, doy_start = top_100_start)
a <- c(300:360, 250:260, 1:30)

cut_doy <- function(doy) {
  neg_doys <- !1:360 %in% sort(doy)
  rle_doys <- rle(neg_doys)
  which.max(rle_doys$lenghts[rle_doys$values])
}

station_means <- splink_geo %>%
  filter(station_id %in% c(83377, 83842, 82900, 83781, 82331, 83783)) %>%
  filter(yearcollected >= 1961) %>%
  mutate(decade = cut_interval(yearcollected, 6, labels = c(1960, 1970, 1980, 1990, 2000, 2010))) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(station_id, decade, search_str) %>%
  summarise(first_mean = mean(circular), n = n()) %>%
  filter(n >= 5) %>%
  group_by(search_str, station_id, decade) %>%
  arrange(decade) %>%
  filter(row_number() %in% 1)
  
test3 <- splink_geo %>%
  filter(station_id %in% c(83377, 83842, 82900, 83781, 82331, 83783)) %>%
  filter(yearcollected >= 1961) %>%
  left_join(select(ungroup(station_means), search_str, station_id, first_mean)) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(station_id, search_str) %>%
  mutate(diff = circular - first_mean) %>%
  ungroup()

ang_diff <- function(x, y) {
  res_all <- c()
  for (i in x) {
    res <- diff(sort(c(i, y), decreasing = TRUE))
    if (abs(res) > 180) {
      res <- res + 360
    } else {
      if (y < i) {
        res <- abs(res)
      } 
    }
  res_all <- c(res_all, res)
  }
  res_all
}

splink_geo %>%
  group_by(station_id, search_str, yearcollected) %>%
  count(sort = T) %>%
  filter(n >= 5) %>%
  group_by(search_str, station_id) %>%
  count(sort = T)

splink_geo %>%
  filter(search_str == "Myrcia guianensis") %>%
  mutate(year = cut_width(yearcollected, width = 3)) %>%
  mutate(circular = to_circular(doy)) %>%
  group_by(year) %>%
  summarise(mean_circ = mean(circular), n = n()) %>%
  filter(n >= 5) %>%
  ggplot(aes(x = year, y = mean_circ, group = 1)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()

splink_geo %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
left_join(splink_geo %>%
            filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  group_by(ecoregion, search_str) %>%
  summarise(mean_circ = mean(circular, na.rm = T), n = n()) %>%
  filter(n >= 30)) %>%
  ggplot(aes(x = mean_circ, y = diff_temp_mean)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ecoregion)

tt4 <- glance %>%
  left_join(
    distinct(
      select(splink_geo, search_str, ecoregion, family_apg)
      ))

splink_geo %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  filter(family_apg %in% c("Poaceae", "Fabaceae", "Asteraceae", "Melastomataceae", "Rubiaceae", "Myrtaceae")) %>%
  ggplot(aes(x = diff_temp_mean, y = doy, col = ecoregion)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") + 
  facet_wrap(~family_apg, scales = "free_y") + 
  theme_bw()

top_families <- splink_geo %>%
  group_by(family_apg) %>%
  count(sort = T) %>%
  ungroup() %>%
  top_n(15)

tt5 <- splink_geo %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  left_join(splink_geo %>%
              filter(search_str %in% top_spp$search_str) %>%
              filter(institutioncode %in% top_inst$institutioncode) %>%
              filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
            #  filter(yearcollected <= 2) %>%
              select(search_str, family_apg, circular, ecoregion, yearcollected, catalognumber, daycollected, collectioncode, diff_temp_mean) %>%
              distinct() %>%
              filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
              group_by(family_apg, ecoregion) %>%
              summarise(hist_mean = mean(circular, na.rm = T), rho = rho.circular(circular, na.rm = T), n = n())) %>%
  filter(n >= 200) %>%
  group_by(ecoregion) %>%
  filter(between(diff_temp_mean, -2.5, 2.5)) %>%
  group_by(family_apg, ecoregion) %>%
  mutate(diff_circular = ang_diff(circular, hist_mean[1])) %>%
  ungroup()

tt5_cor <- tt5 %>%
  group_by(family_apg, ecoregion) %>%
  summarise(cor_temp = cor(diff_temp_mean, diff_circular), cor_year = cor(diff_circular, yearcollected), n = n()) %>%
  group_by(ecoregion) %>%
  ungroup()

p_cor <- tt5_cor %>%
  filter(family_apg %in% (tt5_cor %>% group_by(family_apg) %>% count(sort = T) %>% filter(nn >= 5))$family_apg) %>%
  ggplot(aes(x = ecoregion, y = family_apg, fill = cor_temp)) + 
  geom_tile() + 
  scale_fill_gradient2(name = "r") + 
  theme_bw(base_size = 20) +
  ylab("Family") + 
  xlab("Ecoregion") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave(p_cor, file = "corplot.png", width = 14, height = 12)

splink_geo <- splink_geo %>%
  mutate(life.form = gsub(".*Herb*", "Herb", life.form)) %>%
  mutate(life.form = gsub(".*Woody*", "Woody", life.form)) %>%
 # mutate(life.form = gsub(".*Árvore.*", "Woody", life.form)) %>%
  #mutate(life.form = gsub(".*Subarbusto.*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Liana.*", "Liana", life.form))

tt6 <- splink_geo %>%
  filter(search_str %in% top_spp$search_str) %>%
  filter(institutioncode %in% top_inst$institutioncode) %>%
  filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
  left_join(splink_geo %>%
              filter(search_str %in% top_spp$search_str) %>%
              filter(institutioncode %in% top_inst$institutioncode) %>%
              filter(ecoregion %in% selected_ecoregions$ecoregion) %>%
              filter(yearcollected <= 2000) %>%
              group_by(search_str, ecoregion) %>%
              summarise(hist_mean = mean(circular, na.rm = T), rho = rho.circular(circular, na.rm = T), n = n())) %>%
  filter(n >= 20) %>%
  group_by(ecoregion) %>%
  group_by(search_str, ecoregion) %>%
  mutate(diff_circular = ang_diff(circular, hist_mean[1])) %>%
  ungroup()

tt6_cor <- tt6 %>%
  filter(life.form %in% c("Herb", "Shrub", "Subshrub", "Tree", "Liana")) %>%
  group_by(search_str, ecoregion, life.form) %>%
  summarise(cor_temp = cor(diff_temp_mean, diff_circular), cor_year = cor(doy, yearcollected), n = n()) %>%
  group_by(ecoregion) %>%
  ungroup()

tt6_cor %>%
  ggplot(aes(x = life.form, y = cor_temp)) + 
  geom_boxplot()

ggsave(p_cor2, file = "p_cor2.png", width = 14, height = 12)

top_stations <- stations %>%
  filter(year(start) <= 1975, year(end) >= 2015) %>%
  group_by(station_id) %>%
  arrange(days_missing) %>%
  head(30)

splink_station <- splink_geo %>%
  filter(station_id %in% c(83377, 83842, 82900, 83781, 83738, 82331)) %>%
  select(-id, -season) %>%
  distinct()

### ESTAÇÕES

splink_station %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  ylab("Temperature difference from historical mean (ºC)") + 
  xlab("Year") + 
  facet_wrap(~locality, ncol = 3)

splink_station %>%
  left_join(splink_station %>%
              filter(yearcollected <= 2000) %>%
              group_by(locality, search_str) %>%
              summarise(hist_mean = mean(circular, na.rm = T), rho = rho.circular(circular, na.rm = T), n = n())) %>%
  filter(n >= 10) %>%
  group_by(search_str, ecoregion) %>%
  mutate(diff_circular = ang_diff(circular, hist_mean[1])) %>%
  ungroup() %>%
  ggplot(aes(x = yearcollected, y = diff_circular)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  xlab("Year") + 
  ylab("Difference from historical means (degrees)") + 
  facet_wrap(~locality, ncol = 4) + 
  theme_bw()

splink_station %>%
  left_join(splink_station %>%
              filter(yearcollected <= 2000) %>%
              group_by(locality, search_str) %>%
              summarise(hist_mean = mean(circular, na.rm = T), rho = rho.circular(circular, na.rm = T), n = n())) %>%
  filter(n >= 10) %>%
  group_by(search_str, ecoregion) %>%
  mutate(diff_circular = ang_diff(circular, hist_mean[1])) %>%
  ungroup() %>%
  ggplot(aes(x = diff_temp_mean, y = diff_circular)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed")  +
  xlab("Year") + 
  ylab("Difference from historical means (degrees)") + 
  facet_wrap(~locality, ncol = 3) + 
  theme_bw()

splink_station %>%
  left_join(
    splink_station %>% 
      group_by(locality, search_str) %>% 
      count(sort = T) %>% 
      group_by(locality) %>% 
      top_n(5) %>%
      ungroup()
  ) %>%
  filter(!is.na(n)) %>%
  ggplot(aes(x = diff_temp_mean, y = doy, col = search_str)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~locality, ncol = 3)
  
top_stt <- splink_geo %>%
  group_by(locality, search_str) %>%
  count(sort = T)

filter(top_stt, n >= 30) %>%
  group_by(locality) %>%
  count(sort = T) %>%
  filter(nn >= 20) %>%
  left_join(select(stations, ecoregion, locality, days_missing))

top_brasilia <- splink_geo %>%
  filter(locality == "BRASILIA") %>%
  group_by(search_str) %>%
  summarise(var = var(doy), n = n()) %>%
  ungroup() %>%
  arrange(var) %>%
  filter(n >= 30)

top_all <- splink_geo %>%
  group_by(locality, search_str, ecoregion) %>%
  summarise(var = var(doy), n = n(), n_year = length(unique(yearcollected))) %>%
  ungroup() %>%
  filter(n >= 20) %>%
  arrange(var) %>%
  filter(n_year >= 10)

top_sp <- splink_geo %>%
  filter(locality == "SAO PAULO(MIR.de SANTANA)") %>%
  group_by(search_str) %>%
  summarise(var = var(doy), n = n()) %>%
  ungroup() %>%
  arrange(var) %>%
  filter(n >= 20)

top_diamantina <- splink_geo %>%
  filter(locality == "DIAMANTINA") %>%
  group_by(search_str) %>%
  summarise(var = var(doy), n = n()) %>%
  ungroup() %>%
  arrange(var) %>%
  filter(n >= 20)

top_roncador <- splink_geo %>%
  filter(locality == "RONCADOR") %>%
  group_by(search_str) %>%
  summarise(var = var(doy), n = n()) %>%
  ungroup() %>%
  arrange(var) %>%
  filter(n >= 20)
  
spp_selected <- splink_geo %>%
  mutate(out = abs((doy - mean(doy))/sd(doy)) >= 2.8) %>%
  filter(!out) %>%
  filter(!search_str %in% "Ocotea pulchella") %>%
  mutate(dl = daylength(latitude, doy)) %>%
  right_join(dplyr::select(filter(top_all, var <= 2000 & var >= 100), locality, search_str))

by_spp <- spp_selected %>%
  group_by(search_str) %>%
  nest()

year_model <- function(df) {
  lm(doy ~ yearcollected + temp_mean, df)
}

by_spp <- by_spp %>%
  mutate(model = map(data, year_model))

by_spp <- by_spp %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>%
  arrange(desc(r.squared))

splink_geo %>%
  mutate(dl = daylength(latitude, doy)) %>%
  right_join(select(filter(top_all, var <= 800 & var >= 100), locality, search_str)) %>%
  ggplot(aes(x = dl)) + 
  geom_density(aes(fill = search_str), alpha = 0.2) + 
  geom_vline(xintercept = 12, linetype = "dashed") + 
  # geom_vline(xintercept = 0) + 
  # geom_vline(xintercept = -2, linetype = "dashed") + 
  facet_grid(life.form ~ ecoregion, scales = "free_y") + 
  theme_bw() + 
  theme(legend.position = "none")

tt7 <- splink_geo %>%
  mutate(dl = daylength(latitude, doy)) %>%
  right_join(select(filter(top_all, var <= 800 & var >= 100), locality, search_str)) %>%
  select(family_apg, search_str, latitude, doy, ecoregion, locality, station_id, dl, temp_mean, prec)

splink_geo %>%
  mutate(dl = daylength(latitude, doy)) %>%
  right_join(select(filter(top_all, var <= 800 & var >= 100), locality, search_str)) %>%
  ggplot(aes(x = yearcollected, y = doy, col = search_str)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

splink_geo %>%
  mutate(dl = daylength(latitude, doy)) %>%
  right_join(select(filter(top_all, var <= 800 & var >= 100), locality, search_str)) %>%
  ggplot(aes(x = yearcollected, y = doy, col = life.form)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~search_str, ncol = 5, scales = "free") + 
  theme_bw()

splink_station %>%
  filter(search_str %in% top_roncador$search_str[1:10]) %>%
  ggplot(aes(x = temp_mean, y = doy)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~search_str)

splink_geo %>%
  filter(search_str %in% "Banisteriopsis adenopoda") %>%
  ggplot(aes(x = prec, y = doy)) + 
  geom_point()

splink_station %>%
  filter(locality == "BRASILIA", search_str == "Ouratea hexasperma") %>%
 # lm(doy ~ diff_temp_mean, data = .) %>%
#  summary()
  
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()

esalq %>%
  mutate(data = dmy(data)) %>%
 # filter(year(data) >= 1961) %>%
  ggplot(aes(x = data, y = t_med)) + 
  geom_line(aes(group = 1)) + 
  geom_smooth() + 
  theme_bw()

p_reg_temp <- spp_selected %>%
  ggplot(aes(x = temp_mean, y = doy)) + 
  geom_smooth(method = "lm") + 
  geom_point(size = 0.5) + 
  facet_wrap(~search_str, scales = "free_y") + 
  xlab("Mean temperature (ºC)") + 
  ylab("DOY") + 
  theme_bw()

ggsave(p_reg_temp, file = "p_reg_temp.png", width = 16, height = 12)

p_reg_year <- spp_selected %>%
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_smooth(method = "lm") + 
  geom_point(size = 0.5) + 
  facet_wrap(~search_str, scales = "free") + 
  xlab("Yearcollected") + 
  ylab("DOY") + 
  theme_bw()

ggsave(p_reg_year, file = "p_reg_year.png", width = 16, height = 12)


ggsave(p_coefs, file = "p_coefs.png", width = 10, height = 8)

library(lme4)

spp_selected <- spp_selected %>%
  group_by(search_str) %>%
  mutate(first_year = min(yearcollected)) %>%
  mutate(n_years = length(unique(yearcollected))) %>%
  mutate(var_years = var(yearcollected)) %>%
  ungroup()
  
spp_selected <- spp_selected %>%
  group_by(new_name) %>%
  mutate(prec_scaled = (prec - mean(prec, na.rm = TRUE))/sd(prec, na.rm = TRUE)) %>%
  ungroup()
  
spp_mixed <- lmer(doy ~ temp_mean + prec_scaled +  (1 + temp_mean|new_name) + (0 + prec_scaled|new_name), data = spp_selected, REML = FALSE)

spp_mixed_diff <- lmer(doy ~ diff_temp_mean + prec_scaled +  (1 + diff_temp_mean|new_name) + (0 + prec_scaled|new_name), data = spp_selected, REML = FALSE)

effects <- REsim(spp_mixed)[REsim(spp_mixed)$term=="temp_mean",] %>%
  rename(new_name = groupID) %>%
  mutate(signif = ((median - sd * 1.96) < 0 & (median + sd * 1.96) < 0) | ((median - sd * 1.96) > 0 & (median + sd * 1.96) > 0)) %>%
  mutate(model = "Mean temperature")

effects_diff <- REsim(spp_mixed_diff)[REsim(spp_mixed_diff)$term=="diff_temp_mean",] %>%
  rename(new_name = groupID) %>%
  mutate(signif = ((median - sd * 1.96) < 0 & (median + sd * 1.96) < 0) | ((median - sd * 1.96) > 0 & (median + sd * 1.96) > 0)) %>%
  mutate(model = "Temperature anomaly")

effects_prec <- REsim(spp_mixed)[REsim(spp_mixed)$term=="prec_scaled",] %>%
  rename(new_name = groupID) %>%
  mutate(signif = ((median - sd * 1.96) < 0 & (median + sd * 1.96) < 0) | ((median - sd * 1.96) > 0 & (median + sd * 1.96) > 0)) %>%
  mutate(model = "Mean temperature")

effects_prec_diff <- REsim(spp_mixed_diff)[REsim(spp_mixed_diff)$term=="prec_scaled",] %>%
  rename(new_name = groupID) %>%
  mutate(signif = ((median - sd * 1.96) < 0 & (median + sd * 1.96) < 0) | ((median - sd * 1.96) > 0 & (median + sd * 1.96) > 0)) %>%
  mutate(model = "Temperature anomaly")

effects_life <- filter(spp_life_form, search.str %in% effects$groupID)

p_mixed_others <- effects %>%
  bind_rows(effects_prec, effects_diff, effects_prec_diff) %>%
 # mutate(search.str = paste(new_name, " (", family, ")", sep = "")) %>%
  #mutate(nam = reorder(new_name, -as.numeric(as.factor(family)))) %>%
  mutate(locality = gsub("([A-z]|\\s)*-", "", new_name)) %>%
  mutate(term = case_when(
    term == "prec_scaled" & model == "Mean temperature" ~ "Model 1 - Precipitation",
    term == "prec_scaled" & model == "Temperature anomaly" ~ "Model 2 - Precipitation",
    term == "temp_mean" ~ "Model 1 - Temperature",
    term == "diff_temp_mean" ~ "Model 2 - Temp. Anomaly"
  )) %>%
  filter(locality != "Brasília") %>%
  mutate(new_name = gsub("-.*", "", new_name)) %>%
  left_join(dplyr::select(spp_selected, search_str, family_apg) %>% distinct, by = c("new_name" = "search_str")) %>%
 #mutate(new_name = paste(new_name, "-", family_apg)) %>%
  mutate(new_name = reorder(new_name, -median)) %>%
  ggplot(aes(x = median, y = new_name, col = signif)) + 
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = median - sd * 1.96, xmax = median + sd * 1.96)) + 
  theme_minimal(base_size = 10) + 
  xlab("Median (β)") + 
  ylab("Species") + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "lightgrey")) +
  geom_vline(xintercept = 0, col = "red") + 
  theme_bw() + 
  facet_grid(locality ~ term, scales = "free") + 
  guides(colour = FALSE)

ggsave(p_mixed_others, file = "p_mixed_others.png", width = 9, height = 12)

p_mixed_brasilia <- effects %>%
  bind_rows(effects_prec, effects_diff, effects_prec_diff) %>%
  # mutate(search.str = paste(new_name, " (", family, ")", sep = "")) %>%
  #mutate(nam = reorder(new_name, -as.numeric(as.factor(family)))) %>%
  mutate(locality = gsub("([A-z]|\\s)*-", "", new_name)) %>%
  mutate(term = case_when(
    term == "prec_scaled" & model == "Mean temperature" ~ "Model 1 - Precipitation",
    term == "prec_scaled" & model == "Temperature anomaly" ~ "Model 2 - Precipitation",
    term == "temp_mean" ~ "Model 1 - Temperature",
    term == "diff_temp_mean" ~ "Model 2 - Temp. Anomaly"
  )) %>%
  filter(locality == "Brasília") %>%
  mutate(new_name = gsub("-.*", "", new_name)) %>%
  left_join(dplyr::select(spp_selected, search_str, family_apg) %>% distinct, by = c("new_name" = "search_str")) %>%
#  mutate(new_name = paste(new_name, "-", family_apg)) %>%
#  mutate(new_name = reorder(new_name, -as.numeric(as.factor(family_apg)))) %>%
  mutate(new_name = reorder(new_name, -median)) %>%
  ggplot(aes(x = median, y = new_name, col = signif)) + 
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = median - sd * 1.96, xmax = median + sd * 1.96)) + 
  theme_minimal(base_size = 10) + 
  xlab("Median (β)") + 
  ylab("Species") + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "lightgrey")) +
  geom_vline(xintercept = 0, col = "red") + 
  theme_bw() + 
  facet_wrap(~term, scales = "free_x", ncol = 4) + 
  guides(colour = FALSE)

ggsave(p_mixed_brasilia, file = "p_mixed_brasilia.png", width = 9, height = 12)

coefs_mixed <- coef(spp_mixed_diff)$new_name
coefs_mixed = data.frame(search_str = rownames(coefs_mixed), coefs_mixed)
names(coefs_mixed) <- c("search_str", "inter", "Temperature anomaly", "Standardized precipitation")
coefs_mixed <- coefs_mixed %>%
  left_join(
    spp_selected %>%
      mutate(search_str = paste(search_str, locality, sep = "-")) %>%
      group_by(search_str) %>%
      summarise(mean_doy = mean(doy))
  )

p_coefs <- 
  coefs_mixed %>%
  mutate(locality = gsub("([A-z]|\\s)*-", "", search_str)) %>%
  dplyr::select(-inter) %>%
  gather(variable, value, -search_str, -mean_doy, -locality) %>%
  ggplot(aes(x = value, y = mean_doy)) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = 182.5, linetype = "dashed") + 
  geom_point() + 
  geom_smooth(method = "lm", formula = my.formula) + 
  stat_poly_eq(geom="label", formula = my.formula, 
               parse = TRUE, label.r = unit(0, "cm"), aes(label = ..adj.rr.label..), coef.digits = 2, alpha = 0.9) + 
  theme_bw() + 
  xlab("Slopes") + 
  ylab("Mean flowering DOY") +
  facet_grid(locality ~ variable) + 
  theme(panel.grid = element_blank())

ggsave(p_coefs, file = "p_coefs.png", width = 10, height = 8)

coefs_mixed %>%
  mutate(locality = gsub("([A-z]|\\s)*-", "", search_str)) %>%
  dplyr::select(-inter) %>%
  gather(variable, value, -search_str, -mean_doy, -locality) %>%
  group_by(locality, variable) %>%
  nest() %>%
  mutate(model = map(data, function(x) lm(mean_doy ~ value, data = x))) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  mutate_if(is.numeric, round, 3)
  

p_temp_mean <- spp_selected %>%
  mutate(life.form = gsub(".*Subshrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Shrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Tree*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Herb*", "Herb", life.form)) %>%
  mutate(life.form = gsub(".*Liana*", "Liana", life.form)) %>%
  ggplot(aes(x = temp_mean, group = search_str)) + 
  geom_density(aes(fill = search_str), alpha = 0.2) +
  theme_bw() + 
  ylab("Density") + 
  xlab("Mean temperature (ºC)") + 
  facet_wrap(~life.form, ncol = 1, scales = "free_y") + 
  theme(legend.position = "none")

ggsave(p_temp_mean, file = "p_temp_mean.png", width = 4, height = 8)

p_dl <- spp_selected %>%
  mutate(life.form = gsub(".*Subshrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Shrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Tree*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Herb*", "Herb", life.form)) %>%
  mutate(life.form = gsub(".*Liana*", "Liana", life.form)) %>%
  ggplot(aes(x = dl, group = search_str)) + 
  geom_density(aes(fill = search_str), alpha = 0.2) +
  theme_bw() + 
  ylab("Density") + 
  xlab("Day length (hours)") + 
  facet_wrap(~life.form, ncol = 1, scales = "free_y") + 
  theme(legend.position = "none")

ggsave(p_dl, file = "p_dl.png", width = 4, height = 8)

p_doy <- spp_selected %>%
  mutate(life.form = gsub(".*Subshrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Shrub*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Tree*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Herb*", "Herb", life.form)) %>%
  mutate(life.form = gsub(".*Liana*", "Liana", life.form)) %>%
  ggplot(aes(x = doy, group = search_str)) + 
  geom_density(aes(fill = search_str), alpha = 0.2) +
  theme_bw() + 
  ylab("Density") + 
  xlab("DOY") + 
  facet_wrap(~life.form, ncol = 1, scales = "free_y") + 
  theme(legend.position = "none")

ggsave(p_doy, file = "p_doy.png", width = 4, height = 8)

spp_selected %>%
mutate(life.form = gsub(".*Subarbusto.*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Arbusto.*", "Woody", life.form)) %>%
  mutate(life.form = gsub(".*Árvore.*", "Woody", life.form)) %>%
  mutate(life.form = gsub("Erva", "Herb", life.form)) %>%
  mutate(life.form = gsub(".*Liana.*", "Liana", life.form)) %>%
  ggplot(aes(x = yearcollected, y = diff_temp_mean, col = life.form)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", alpha = 0.2)

spp_selected %>%
  ggplot(aes(y = doy, x = yearcollected, group = 1)) + 
  geom_smooth(method = "lm") + 
  geom_point()+
  facet_wrap(~search_str)

spp_selected %>%
  filter(!out, locality == "Brasília") %>%
  group_by(search_str) %>%
  ungroup() %>%
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_point(aes(col = out)) + 
  stat_poly_eq(geom = "label", formula = my.formula, parse = TRUE, label.r = unit(0, "cm"), coef.digits = 2, alpha = 0.9,
               aes(label =..rr.label..)) + 
  geom_smooth( method = "lm", formula = my.formula) + 
  facet_wrap(~search_str)

spp_selected %>%
  group_by(search_str, family_apg) %>%
  summarise(cor_temp_mean = cor(doy, temp_mean), cor_diff_temp_mean = cor(doy, diff_temp_mean), cor_year = cor(doy, yearcollected), cor_prec = cor(doy, prec_scaled)) %>%
  ungroup() %>%
  gather(variable, value, -search_str, -family_apg) %>%
  mutate(search_str = paste(search_str, family_apg)) %>%
  mutate(search_str = reorder(search_str, value)) %>%
  ggplot(aes(x = variable, y = search_str, fill = value)) + 
  geom_raster() + 
  scale_fill_gradient2()

spp_selected <- spp_selected %>%
  mutate(locality = gsub(".*CATA.*", "Catalão", locality)) %>%
  mutate(locality = gsub(".*CURI.*", "Curitiba", locality)) %>%
  mutate(locality = gsub(".*PAUL.*", "São Paulo", locality)) %>%
  mutate(locality = gsub(".*IRA.*", "Irati", locality)) %>%
  mutate(locality = gsub(".*RESE.*", "Resende", locality)) %>%
  mutate(locality = gsub(".*FRA.*", "Franca", locality)) %>%
  mutate(locality = gsub(".*BRA.*", "Brasília", locality)) %>%
  filter(!locality %in% c("Irati", "Resende", "Franca")) %>%
  mutate(new_name = paste(search_str, locality, sep = "-")) %>%
  filter(search_str != "Ocotea pulchella")

p_stations_lm <- spp_selected %>%
  ggplot(aes(x = yearcollected, y = doy)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = my.formula) + 
  stat_poly_eq(geom="label", formula = my.formula, 
               parse = TRUE, label.r = unit(0, "cm"), aes(label = ..adj.rr.label..), coef.digits = 2, alpha = 0.9) +
  facet_wrap(~locality) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab("Year") + 
  ylab("Day of the year")

ggsave(p_stations_lm, file = "p_stations_lm.png", width = 8, height = 6)

p_stations_lm_temp <- spp_selected %>%
  ggplot(aes(x = diff_temp_mean, y = doy)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = my.formula) + 
  stat_poly_eq(geom="label", formula = my.formula, 
               parse = TRUE, label.r = unit(0, "cm"), aes(label = ..adj.rr.label..), coef.digits = 2, alpha = 0.9) +
  facet_wrap(~locality) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab("Mean temperature anomaly (ºC)") + 
  ylab("Day flowering") + 

ggsave(p_stations_lm_temp, file = "p_stations_lm_temp.png", width = 8, height = 6)


lm_spp <- spp_selected %>%
  group_by(locality, search_str, family_apg) %>%
  nest() %>%
  mutate(model = map(data, model)) %>%
  mutate(glance = map(model, broom::tidy)) %>%
  unnest(glance, .drop = TRUE) %>%
  filter(term == "yearcollected") %>%
  left_join(
    spp_selected %>%
      group_by(locality, search_str, family_apg) %>%
      nest() %>%
      mutate(model = map(data, model)) %>%
      mutate(glance = map(model, broom::glance)) %>%
      unnest(glance, .drop = TRUE), by = c("search_str", "family_apg", "locality")) %>%
  arrange(desc(adj.r.squared))

lm_stations_temp <- spp_selected %>%
  group_by(locality) %>%
  nest() %>%
  mutate(model = map(data, model_temp)) %>%
  mutate(glance = map(model, broom::tidy)) %>%
  unnest(glance, .drop = TRUE) %>%
  filter(term == "diff_temp_mean") %>%
  left_join(
    spp_selected %>%
      group_by(locality) %>%
      nest() %>%
      mutate(model = map(data, model_temp)) %>%
      mutate(glance = map(model, broom::glance)) %>%
      unnest(glance, .drop = TRUE), by = c("locality")) %>%
  arrange(desc(adj.r.squared)) %>%
  mutate_if(is.numeric, round, 3)


by_station <- spp_selected %>%
  group_by(search_str, locality) %>%
  nest()

model <- function(df) {
  lm(doy ~ yearcollected, data = df)
}

model_temp <- function(df) {
  lm(doy ~ diff_temp_mean, data = df)
}

by_station <- by_station %>%
  mutate(model = map(data, model))

glance <- by_station %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

model_station_temp <- function(df) {
  lm(temp_mean ~ yearcollected, data = df)
}

model_station_prec <- function(df) {
  lm(prec ~ yearcollected, data = df)
}

station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>% 
  mutate(yearcollected = year(date)) %>%
  group_by(locality) %>%
  nest() %>%
  mutate(model = map(data, model_station_temp)) %>%
  mutate(glance = map(model, broom::tidy)) %>%
  unnest(glance, .drop = TRUE) %>%
  mutate_if(is.numeric, round, 3)

station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>% 
  mutate(yearcollected = year(date)) %>%
  group_by(locality) %>%
  nest() %>%
  mutate(model = map(data, model_station_prec)) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  mutate_if(is.numeric, round, 3)
  

spp_selected %>%
  group_by(search_str) %>%
  summarise(mean = mean(doy)) %>%
  ggplot(aes(x = mean)) +
  geom_histogram(binwidth = 10) + 
  xlab("Mean DOY") +
  ylab("Count") + 
  theme_bw()

summary(lmer(doy ~ yearcollected + (0 + yearcollected|locality), data = spp_selected, REML = TRUE))

p_clim_temp <- station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>%
ggplot(aes(x = date, y = temp_mean)) + 
  geom_point(size = 0.3) + 
  geom_line(data = . %>%
              group_by(date = floor_date(date, "month"), locality) %>%
              summarise(temp_mean = mean(temp_mean, na.rm = TRUE)),
            aes(x = date, y = temp_mean), col = "red"
  ) + 
  geom_smooth(method = "lm", formula = my.formula) +
#  stat_poly_eq(geom="label", formula = my.formula, 
#               parse = TRUE, label.r = unit(0, "cm"), aes(label = ..adj.rr.label..), coef.digits = 2, alpha = 0.9) +
  facet_wrap(~locality, ncol = 4) + 
  theme_bw() + 
  xlab(NULL) + 
  ylab("Mean temperature (ºC)") + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = 1))


p_clim_rain <- station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  group_by(date = floor_date(date, "month"), locality) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>%
ggplot(aes(x = date, y = prec)) + 
  geom_line() +
  geom_smooth(method = "lm", formula = my.formula) +
 # stat_poly_eq(geom="label", formula = my.formula, 
#               parse = TRUE, label.r = unit(0, "cm"), aes(label = ..adj.rr.label..), coef.digits = 2, alpha = 0.9) +
  facet_wrap(~locality, ncol = 4) + 
  theme_bw() + 
  xlab("Date") + 
  ylab("Precipitation (mm)") + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(grid.arrange(p_clim_temp, p_clim_rain, ncol = 1), file = "climate.png", width = 11, height = 5)


p_box_temp <- station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>%
  mutate(Month = month(date, label = TRUE), year = year(date)) %>%
  group_by(locality, Month, year) %>%
  summarise(prec = sum(prec, na.rm = TRUE), temp_mean = mean(temp_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(locality, temp_mean, prec, Month) %>%
  ggplot(aes(x = Month, y = temp_mean)) +
  geom_boxplot() + 
  xlab(NULL) + 
  ylab("Mean temperature (ºC)") + 
  facet_wrap(~locality, ncol = 4) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p_box_rain <- station_data_summary %>% 
  left_join(dplyr::select(stations, station_id, locality)) %>%
  filter(locality %in% c("BRASILIA", "CATALAO", "CURITIBA", "SAO PAULO(MIR.de SANTANA)")) %>%
  mutate(locality = case_when(
    locality == "BRASILIA" ~ "Brasília",
    locality == "CATALAO" ~ "Catalão",
    locality == "CURITIBA" ~ "Curitiba",
    locality == "SAO PAULO(MIR.de SANTANA)" ~ "São Paulo"
  )) %>%
  mutate(Month = month(date, label = TRUE), year = year(date)) %>%
  group_by(locality, Month, year) %>%
  summarise(prec = sum(prec, na.rm = TRUE), temp_mean = mean(temp_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(locality, temp_mean, prec, Month) %>%
  ggplot(aes(x = Month, y = prec)) +
  geom_boxplot() + 
  ylab("Total rainfall (mm)") + 
  facet_wrap(~locality, ncol = 4) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


ggsave(grid.arrange(p_box_temp, p_box_rain, ncol = 1), file = "p_box.png", width = 10, height = 6)

spp_orders <- spp_selected %>%
  group_by(locality, search_str) %>% 
  summarise(mean = mean(doy)) %>% 
  arrange(mean)

p_locality <- function(x) {
  spp_selected %>%
  filter(locality == x) %>%
  mutate(search_str = factor(search_str,
                              levels = rev(filter(spp_orders, locality == x)$search_str))) %>% 
  ggplot(aes(y = doy, x = search_str)) + 
  geom_hline(yintercept = 182, col = "red", linetype = "dashed") + 
  geom_boxplot(alpha = 0.2) + 
  theme_bw() + 
  coord_flip() + 
  xlab("Species") + 
  ylab("Day of the year")
}

ggsave(p_locality("Brasília"), file = "brasilia.png", width = 5, height = 12)
ggsave(p_locality("Catalão"), file = "catalão.png", width = 5, height = 8)
ggsave(p_locality("São Paulo"), file = "sao_paulo.png", width = 5, height = 8)
ggsave(p_locality("Curitiba"), file = "curitiba.png", width = 5, height = 9)

p_year_bras <- spp_selected %>%
  filter(locality == "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = yearcollected, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Year") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

p_temp_bras <- spp_selected %>%
  filter(locality == "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = temp_mean, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~name_abb) + 
  ylab("Day of the year") + 
  xlab("Temperature (ºC)") + 
  theme(strip.text = element_text(face = "italic"))

p_rainfall_bras <- spp_selected %>%
  filter(locality == "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = prec, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Total rainfall (mm)") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

p_diff_bras <- spp_selected %>%
  filter(locality == "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = diff_temp_mean, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Temperature anomaly (ºC)") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

ggsave(p_year_bras, file = "p_year_bras.png", width = 12, height = 12)
ggsave(p_rainfall_bras, file = "p_rainfall_bras.png", width = 12, height = 12)
ggsave(p_temp_bras, file = "p_temp_bras.png", width = 12, height = 12)
ggsave(p_diff_bras, file = "p_diff_bras.png", width = 12, height = 12)

p_year_others <- spp_selected %>%
  filter(locality != "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = yearcollected, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Year") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

p_temp_others <- spp_selected %>%
  filter(locality != "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = temp_mean, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(~name_abb) + 
  ylab("Day of the year") + 
  xlab("Temperature (ºC)") + 
  theme(strip.text = element_text(face = "italic"))

p_rainfall_others <- spp_selected %>%
  filter(locality != "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = prec, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Total rainfall (mm)") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

p_diff_others <- spp_selected %>%
  filter(locality != "Brasília") %>%
  mutate(name_abb = gsub("[a-z]+(?=\\s)", "\\.", search_str, perl = T)) %>%
  ggplot(aes(x = diff_temp_mean, y = doy)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ylab("Day of the year") + 
  xlab("Temperature anomaly (ºC)") + 
  facet_wrap(~name_abb) + 
  theme(strip.text = element_text(face = "italic"))

ggsave(p_year_others, file = "p_year_others.png", width = 12, height = 12)
ggsave(p_rainfall_others, file = "p_rainfall_others.png", width = 12, height = 12)
ggsave(p_temp_others, file = "p_temp_others.png", width = 12, height = 12)
ggsave(p_diff_others, file = "p_diff_others.png", width = 12, height = 12)
