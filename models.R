# Models

by_species <- as.data.frame(splink_without_dups) %>%
  filter(search_str %in% doys$search_str) %>%
  filter(grepl("flor(?!esta)|p[é|e]tala|flower|antese|corola|corolla|fl\\.", notes, perl = TRUE)) %>%
  group_by(search_str) %>%
  nest()

spp_model <- function(df) {
  lm(doy ~ latitude, data = df)
}

spp_model_rmse <- function(data, model) {
  m <- fitted(model)
  o <- data$doy
  sqrt(mean((m - o)^2))
}

by_species <- by_species %>% 
  mutate(model = purrr::map(data, spp_model))

by_species <- by_species %>% 
  mutate(rmse = map2(data, model, spp_model_rmse))

rmse <- unnest(by_species, rmse)

rmse <- rmse %>%
  arrange(rmse)

rmse <- unnest(rmse, data)

rmse %>%
  filter(search_str == "Prestonia erecta") %>%
  ggplot(aes(x = yearcollected, y = doy_april)) + 
  geom_point(alpha = 0.2) + geom_smooth(method = "lm")

glance <- by_species %>%
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) %>%
  arrange(desc(adj.r.squared))

glance

glance <- glance %>%
  filter(p.value < 0.05) %>%
  arrange(desc(adj.r.squared))


by_species <- by_species[match(glance$search_str, by_species$search_str), ]
          