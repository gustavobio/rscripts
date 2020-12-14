require(sf)
library(modelr)
library(tidyverse)
library(viridis)
require(circular)
require(grid)
require(gridExtra)
require(ggridges)

ecoregions_splink <- lapply(st_intersects(filter(splink_coords, id %in% splink$id), ecoregions_br), function(x) ifelse(length(x) == 1, x, NA))
splink$ecoregion <- as.character(ecoregions_br$ECO_NAME[unlist(ecoregions_splink)])

eco_counts <- splink %>%
  select(ecoregion) %>%
  count(ecoregion, sort = TRUE)

ecoregions_br <- left_join(ecoregions_br, eco_counts, by = c("ECO_NAME" = "ecoregion"))

spp_counts <- splink %>%
  filter(taxon_rank == "species") %>%
  group_by(ecoregion, search_str) %>%
  count() %>%
  ungroup() %>%
  group_by(ecoregion) %>%
  filter(n >= 20)

splink_eco <- left_join(splink, spp_counts)

splink_eco <- filter(splink_eco, !is.na(n))
ecoregions_br <- ecoregions_br %>%
  mutate(area = as.numeric(st_area(.))) %>%
  mutate(dens = n/(area/10^6))

splink_eco <- splink_eco %>%
  mutate(circular = to_circular(doy))

p_fig01 <- ggplot(filter(ecoregions_br, ECO_NAME %in% selected_ecoregions$ecoregion)) + 
  geom_sf(data = br, size = 0.2) + 
  geom_sf(aes(fill = ECO_NAME), size = 0.2, colour = "black") + 
  geom_point(aes(x = longitude, y = latitude), data = filter(splink_eco, search_str %in% spp_counts$search_str, ecoregion %in% selected_ecoregions$ecoregion), size = 0.03, alpha = 0.3) + 
  geom_point(data = filter(stations, ecoregion %in% selected_ecoregions$ecoregion), aes(x = longitude, y = latitude), size = 3, col = "black", shape = 24, fill = "white") + 
  theme_bw() + 
  xlab(NULL) + ylab(NULL) +
  scale_fill_discrete(name = "Ecoregion")

ggsave(p_fig01, file = "p_fig01.png", width = 12, height = 8)
ggsave(p_ecoregions_number, file = "~/Desktop/p_ecoregions_number.pdf", width = 12, height = 12)

p_ecoregions_number <- ggplot(ecoregions_br) + 
  geom_sf(data = br, size = 0.2) + 
  geom_sf(aes(fill = n), size = 0.2, colour = "black") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis("Número de indivíduos", 
                     na.value = "grey90", 
                     labels = scales::format_format(big.mark = ".", decimal.mark = ",")) +
  guides(fill = guide_colorbar(barwidth = 30, title.position = "top"))
ggsave(p_ecoregions_number, file = "~/Desktop/p_ecoregions_number.pdf", width = 12, height = 12)

plot_eco <- function(name) {
  p1 <- ggplot(filter(ecoregions_br, ECO_NAME == name)) +
    geom_sf(data = br, size = 0.2, fill = "white") +
    geom_sf(fill = "darkgrey",
            size = 0.2,
            colour = "black") +
    theme_bw()
  
  data <- splink_eco %>%
    filter(n >= 10, ecoregion %in% name) %>%
    mutate(circular = suppressWarnings(as.circular(
      doy * (360 / 366),
      units = "degrees",
      rotation = "clock",
      modulo = "2pi"
    ))) %>%
    group_by(search_str)
    sum
  p2 <- data %>%
    summarise(mean = mean.circular(circular), rho = rho.circular(circular)) %>%
    filter(rho >= 0.20) %>%
    ggplot() +
    geom_histogram(aes(mean), bins = 25) +
    ylab("Count") + 
    xlab("Circular mean") + 
    theme_bw()
  
  p3 <- data %>%
    summarise(rho = rho.circular(circular)) %>%
    ggplot() +
    geom_histogram(aes(rho), bins = 25) +
    ylab("Count") + 
    xlab("Rho") + 
    theme_bw()
  #layout = rbind(c(1, 1, 1, 2, 2), c(1, 1, 1, 3, 3))
  layout = rbind(c(1, 1, 2, 2, 3, 3), c(1, 1, 2, 2, 3, 3), c(1, 1, 2, 2, 3, 3))
  grid.arrange(p1, p2, p3, top = name, layout_matrix = layout)
}

quartz()

plot_all <- function(names) {
  plots <- sapply(names, plot_eco)
  grid.arrange(grobs = plots, ncol = 1)
}

res <- try(plot_all(selected_ecoregions$ecoregion))
while(inherits(res, "try-error")) {
  res <- try(plot_all(selected_ecoregions$ecoregion))
}

filter(splink_eco, ecoregion %in% selected_ecoregions) %>%
  filter(n >= 40) %>%
  group_by(ecoregion, search_str) %>%
  summarise(mean = mean.circular(circular)) %>%
  ungroup() %>%
  ggplot(aes(x = mean, y = ecoregion)) +
  geom_density_ridges(from = 0, to = 360) + 
  theme_bw() 
  
ggplot(ecoregions_br) + 
  geom_sf(data = br, size = 0.2) + 
  geom_sf(aes(fill = dens), size = 0.2, colour = "black") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis(Densidade~de~indivíduos~(ind/km^2), 
                     na.value = "grey90") +
  guides(fill = guide_colorbar(barwidth = 30, title.position = "top"))

by_ecoregions <- splink_eco %>%
  group_by(ecorregion, search_str) %>%
  nest()

selected_ecoregions <- spp_counts %>%
  count(ecoregion, sort = T) %>%
  ungroup() %>%
  top_n(8)

splink_eco %>%
  filter(n >= 10, ecorregion %in% glance_count$ecorregion) %>%
  mutate(circular = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "2pi")) %>%
  group_by(ecorregion, search_str) %>%
  summarise(mean = mean.circular(circular)) %>%
  ggplot() + 
  geom_histogram(aes(mean)) + 
  facet_wrap(~ecorregion, scales = "free_y", ncol = 1) + 
  theme_bw()

as.data.frame(splink_eco) %>%
  filter(n >= 20, ecorregion %in% glance_count$ecorregion) %>%
  filter(!is.na(search_str), taxon_rank == "species") %>%
  filter(identifiedby != "") %>%
  group_by(search_str, ecorregion) %>%
  summarise(nn = n()) %>%
  ungroup() %>%
  right_join(splink_eco) %>%
  filter(n >= 20, ecorregion %in% glance_count$ecorregion) %>%
  mutate(circular = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "2pi")) %>%
  group_by(ecorregion, search_str) %>%
  summarise(sd = mean(circular)) %>%
  ggplot() + 
  geom_histogram(aes(sd), bins = 20) + 
  xlab("Mean date") + 
  facet_wrap(~ecorregion, ncol = 1) + 
  theme_bw()

splink_eco %>%
  filter(n >= 10, ecorregion %in% glance_count$ecorregion) %>%
  mutate(circular = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "pi")) %>%
  group_by(ecorregion, search_str) %>%
  summarise(rho = rho.circular(circular)) %>%
  ggplot() + 
  geom_histogram(aes(rho)) + 
  facet_wrap(~ecorregion, scales = "free_y", ncol = 1) + 
  theme_bw()

splink_eco %>%
  filter(n >= 10, ecorregion %in% glance_count$ecorregion) %>%
  mutate(circular = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "pi")) %>%
  group_by(ecorregion, search_str) %>%
  summarise(rho = rho.circular(circular), sd = sd.circular(circular)) %>%
  ggplot() + 
  geom_point(aes(x = rho, y = sd)) 


year_model <- function(df) {
  lm(doy ~ yearcollected, data = df)
}

by_ecorregions <- by_ecorregions %>% 
  mutate(model_year = map(data, year_model))

glance <- by_ecorregions %>% 
  mutate(glance = map(model_year, function(x) coef(x)[[2]])) %>% 
  unnest(glance, .drop = TRUE)

glance_count <- glance %>% count(ecorregion) %>% filter(n > 100)

glance %>%
  filter(ecorregion %in% glance_count$ecorregion) %>%
  filter(between(glance,-5, 5)) %>%
  ggplot() +
  geom_histogram(aes(glance)) +
  geom_vline(xintercept = 0, colour = "red") +
  facet_wrap( ~ ecorregion, scales = "free_y") + 
  theme_bw()

glance %>%
  group_by(ecorregion) %>%
  summarise(coef = mean(glance, na.rm = T)) %>%
  right_join(ecorregions_br, by = c("ecorregion" = "ECO_NAME")) %>%
  filter(coef >= -5) %>%
  ggplot() + 
  geom_sf(data = br_estados, size = 0.2) +
  geom_sf(data = br, size = 0.2) + 
  geom_sf(aes(fill = coef), size = 0.2, colour = "black") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_gradient2(na.value = "black")

splink %>%
  group_by(search_str, ecorregion) %>%
  mutate(year_group = cut_width(yearcollected, 5)) %>%
  ungroup() %>%
  filter(search_str == "Lantana camara", yearcollected >= 1975, ecorregion == "Cerrado") %>%
  group_by(year_group) %>%
  summarise(circular = mean(as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "2pi", zero = 0))) %>%
  ggplot(aes(x = year_group, y = circular)) +
  geom_point() +
  geom_smooth()

splink_test <- filter(splink, search_str %in% c("Myrcia guianensis", "Lantana camara"))

mean_ci <- function(x) {
  mean_vector <- mean(x)
  rho_vector <- rho.circular(x)
  rho_squared <- rho_vector^2
  n <- length(x)
  div <- 2 * n * rho_squared
  t2bar <- trigonometric.moment(x, p = 2, center = TRUE)
  bbar2 <- 0
  mubc <- mean_vector + (bbar2/div)
  mean_vector_se <- sqrt((1 - t2bar$cos)/div)
  mean_ci_up <- mubc + qnorm(1-0.05/2) * mean_vector_se
  mean_ci_down <- mubc - qnorm(1-0.05/2) * mean_vector_se
  c(conversion.circular(mean_ci_down, units = "degrees"), mean_vector, conversion.circular(mean_ci_up, units = "degrees"))
}

circ_ci <- function(x) {
  as.numeric(conversion.circular(ConfIntBoot(x, 0, 95, 1000)[[1]], units = "degrees", modulo = "asis", rotation = "clock"))
}

splink_eco1 <- splink_eco

splink_eco <- splink_eco %>%
  filter(!is.na(search_str), taxon_rank == "species", ecoregion %in% selected_ecoregions$ecoregion) %>%
  group_by(ecoregion, search_str) %>%
  filter(yearcollected > 1970, yearcollected <= 2015) %>%
  mutate(year_group = cut_width(yearcollected, 5, closed = "left"))

splink_selected <- splink_eco %>%
  group_by(ecoregion, search_str, year_group) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n >= 8) %>%
  group_by(ecoregion, search_str) %>%
  summarise(nn = n()) %>%
  ungroup() %>%
  filter(nn >= 6) %>%
  left_join(splink_eco, by = c("ecoregion", "search_str")) %>%
  group_by(search_str, ecoregion, year_group) %>%
  summarise(n = n(), rho = rho.circular(circular), mean = mean.circular(circular)) %>%
  filter(!is.na(mean)) %>%
  mutate(season = case_when(
    mean(mean) > 330 | mean(mean) <= 60 ~ "summer",
    mean(mean) > 60 & mean(mean) <= 150 ~ "fall",
    mean(mean) > 150 & mean(mean) <= 240 ~ "winter",
    mean(mean) > 240 & mean(mean) <= 330 ~ "spring"
  )) %>%
  mutate(mean = as.numeric(mean)) %>%
  mutate(new_mean = case_when(
    season == "summer" ~ ifelse(mean > 180, mean - 180, mean + 360 - 180),
    season == "fall" ~ ifelse(mean > 270, mean - 270, mean + 360 - 270),
    season == "winter" ~ mean,
    season == "spring" ~ ifelse(mean > 120, mean - 120, mean + 360 - 120)
  )) %>%
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))

p_rho_seasons <- splink_selected %>%
  ggplot(aes(x = year_group, y = rho)) + 
  geom_point(aes(colour = search_str)) + 
  geom_line(size = 0.1) + 
  #geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_rho_seasons, file = "~/Desktop/p_rho_seasons.pdf", width = 12, height = 12)

p_rho_ml_seasons <- splink_selected %>%
  ggplot(aes(x = year_group, y = rho, colour = search_str)) + 
  geom_point() + 
  #geom_line(size = 0.1) + 
  geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_rho_ml_seasons, file = "~/Desktop/p_rho_ml_seasons.pdf", width = 12, height = 12)

p_rho_ml_seasons_group1 <- splink_selected %>%
  ggplot(aes(x = year_group, y = rho, group = 1)) + 
  geom_point(aes(colour = search_str)) + 
  #geom_line(size = 0.1) + 
  geom_smooth(method = "lm", se = T) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_rho_ml_seasons_group1, file = "~/Desktop/p_rho_ml_seasons_group1.pdf", width = 12, height = 12)

p_mean_seasons <- splink_selected %>%
  ggplot(aes(x = year_group, y = mean)) + 
  geom_point(aes(colour = search_str)) + 
  geom_line(size = 0.1) + 
  #geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_mean_seasons, file = "~/Desktop/p_mean_seasons.pdf", width = 12, height = 12)

p_new_mean_seasons <- splink_selected %>%
  ggplot(aes(x = year_group, y = new_mean)) + 
  geom_point() + 
  geom_line(size = 0.1) + 
  #geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_new_mean_seasons, file = "~/Desktop/p_new_mean_seasons.pdf", width = 12, height = 12)

p_new_mean_ml_seasons <- splink_selected %>%
  ggplot(aes(x = year_group, y = new_mean)) + 
  geom_point(aes(col = search_str)) + 
  #geom_line(size = 0.1) + 
  geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_new_mean_ml_seasons, file = "~/Desktop/p_new_mean_ml_seasons.pdf", width = 12, height = 12)

p_new_mean_ml_seasons_group1 <- splink_selected %>%
  ggplot(aes(x = year_group, y = new_mean, group = 1)) + 
  geom_point(aes(col = search_str)) + 
  #geom_line(size = 0.1) + 
  geom_smooth(method = "lm", se = T) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0))
ggsave(p_new_mean_ml_seasons_group1, file = "~/Desktop/p_new_mean_ml_seasons_group1.pdf", width = 12, height = 12)

splink_selected %>%
  ggplot(aes(x = year_group, y = new_mean, colour = search_str)) + 
  geom_point() + 
  geom_line(size = 0.1) + 
  geom_smooth(method = "lm", se = F, alpha = 0.2, size = 0.1) + 
  theme_bw() + 
  facet_grid(ecoregion~season) + 
  theme(legend.position = "none")

tt1 <- as.data.frame(splink) %>%
  filter(!is.na(search_str), taxon_rank == "species") %>%
  count(search_str, ecorregion, sort = TRUE) %>%
  right_join(splink) %>%
  filter(n > 20) %>%
  mutate(circular = as.circular(doy * (360/366), units = "degrees", rotation = "clock", modulo = "2pi", zero = 0, type = "angles")) %>%
  group_by(search_str, ecorregion) %>%
  mutate(year_group = cut_width(yearcollected, 10)) %>%
  ungroup() %>%
  filter(yearcollected >= 1970, yearcollected <= 2015, search_str == "Bowdichia virgilioides")


spp_eco1 <- splink_eco %>%
  filter(taxon_rank == "species", ecoregion %in% selected_ecoregions$ecoregion) %>%
  filter(search_str %in% spp_counts$search_str) %>%
  group_by(search_str, ecoregion) %>%
  summarise(mean = mean(circular, na.rm = TRUE), rho = rho.circular(circular, na.rm = TRUE)) %>%
  ungroup()

filter(spp_eco1, rho <= 0.05)
  
filter(splink_eco, search_str == "Achetaria platychila", ecoregion == "Bahia Forests") %>%
  ggplot(aes(x = doy)) + 
  geom_histogram()

spp_eco1 %>%
  filter(rho >= 0.20, ecoregion == "Cerrado") %>%
  ggplot(aes(x = mean)) +
  geom_histogram(bins = 25) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 330, by = 30), labels = month.abb, limits = c(0, 360))  

filter(splink_eco, institutioncode == "NYBG") %>% 
  filter(str_detect(notes, "phenology")) %>%
  dim()

