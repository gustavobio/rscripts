
spp_with_flowers <- function(spp) {
  grep("(?<!a)flor(?!esta)|p[é|e]tala|flower|antese|corola|corolla|cálice", spp$notes, ignore.case = TRUE, perl = TRUE)
}

spp_without_flowers <- function(spp) {
  grep("est[e|é]ril|vegetativ|sterile", spp$notes, ignore.case = TRUE, perl = TRUE)
}

big <- filter(as.data.frame(splink), family_apg == "Bignoniaceae")

big_with <- spp_with_flowers(big)
big_without <- spp_without_flowers(big)

big$pheno <- "unknown"
big$pheno[big_with] <- "with flower"
big$pheno[big_without] <- "without flower"

top_5_spp <- big %>% group_by(search_str) %>% count() %>% ungroup() %>% arrange(desc(n)) %>% top_n(15) %>% select(search_str)

ggplot(filter(as.data.frame(splink), search_str %in% "Ruellia asperula"), aes(x = pheno, y = doy)) + 
  geom_point(aes(fill = latitude), size = 1.2, shape = 21, stroke = 0) + 
  theme_minimal() + 
  facet_wrap(~search_str)

splink_summary <- as.data.frame(splink) %>%
  filter(search_str != "") %>%
  group_by(search_str) %>%
  summarise(n = n(),
            mean_lat = mean(latitude), 
            mean_doy = mean(doy),
            sd_lat = sd(latitude),
            sd_doy = sd(doy)
  ) %>%
  ungroup() %>%
  arrange(desc(n))
 
as.data.frame(splink) %>%
  filter(search_str == "Ruellia asperula") %>%
  ggplot() +
  geom_sf(data = br, fill = "white") +
  theme_minimal() + 
  geom_point(aes(x = longitude, y = latitude), size = 1) +
  labs(x = "", y = "") 
