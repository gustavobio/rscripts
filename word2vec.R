require(dplyr)
require(stringr)
require(tidytext)
require(wordVectors)
require(tidyverse)

flores <- splink %>%
  mutate(notes = tolower(notes)) %>%
  filter(str_detect(notes, "[F|f]lor(?!a|esta|ula)|(\\s|^)[P|p][é|e]tala|[F|f]lower(?!ed)|[A|a]ntese|[C|c]orola|[C|c]orolla|[F|f]l\\.")) %>%
  mutate(notes = gsub("\\.|,|;|\\:", " ", notes))

palavras <- splink %>%
  unnest_tokens(token, notes, token = "ngrams", n = 2) %>%
  select(search_str, scientific_name, family_apg, threat_status, longitude, latitude, barcode, token) %>%
  mutate(token = tolower(token))

palavras <- palavras %>%
  separate(token, c("palavra1", "palavra2"), sep = " ")

palavras <- palavras %>%
  filter(str_detect(palavra1, "flor(?!esta)|p[é|e]tala|flower|antese|corola|corolla|fl\\."))

token_count <- palavras %>%
  group_by(palavra1, palavra2) %>%
  count(sort = T)

cat(splink$notes, file = "test.txt")
model <- train_word2vec("test.txt","splink10.bin", vectors=200, threads=4, window=3, iter=10, negative_samples=0, min_count = 20)

flores <- closest_to(model, "flores", 10)$word
cores <- unique(
  c(
    closest_to(model, "amarelas", 20)$word,
    closest_to(model, "brancas", 20)$word,
    closest_to(model, "vermelhas", 20)$word,
    closest_to(model, "rosas", 20)$word,
    closest_to(model, "laranjas", 20)$word,
    closest_to(model, "azuis", 20)$word,
    closest_to(model, "pretas", 4)$word,
    closest_to(model, "marrons", 3)$word
  )
)

tokens <- splink %>%
  unnest_tokens(token, notes, token = "ngrams", n = 2) %>%
  separate(token, c("primeira_palavra", "segunda_palavra"), sep = " ") %>%
  filter(primeira_palavra %in% flores, segunda_palavra %in% cores) %>%
  distinct()

cores_traduzidas <- c(
  "amarelas" = "yellow", 
  "brancas"= "white", 
  "roxas" = "purple", 
  "alvas"  ="white", 
  "róseas" = "pink1", 
  "alaranjadas" = "orange", 
  "amareladas" = "lightyellow", 
  "vermelhas" = "red", 
  "amarelo-alaranjadas" = "goldenrod", 
  "amarelo-claras" = "lightyellow2", 
  "lilases" = "violet",
  "rosas" = "pink1",
  "amarelo-pálidas" = "lightyellow2",
  "amrelas" = "yellow",
  "cremes" = "wheat4",
  "marelas" = "yellow",
  "creme-amareladas" = "lightgoldenrodyellow",
  "laranjas" = "darkorange",
  "azuladas" = "blueviolet",
  "roseas" = "lightpink",
  "esbranquiçadas" = "wheat2",
  "branco-esverdeadas" = "palegreen",
  "branco-amareladas" = "lightyellow",
  "creme-esverdeadas" = "darkseagreen3",
  "lilazes" = "violet",
  "amarelo-esverdeadas" = "greenyellow",
  "branco-rosadas" = "lightpink",
  "azuis" = "slateblue4",
  "vermelho-alaranjadas" = "orangered1",
  "rosadas" = "pink",
  "avermelhadas" = "tomato",
  "arroxeadas" = "thistle2",
  "vináceas" = "deeppink4",
  "purpúreas" = "violetred",
  "vinosas" = "deeppink4",
  "esverdeadas" = "palegreen",
  "violáceas" = "violet",
  "liláses" = "violet",
  "violetas" = "violet",
  "laranja" = "darkorange",
  "laranjadas" = "orange1",
  "laranja-avermelhadas" = "orangered1",
  "amarelo-avermelhadas" = "darkorange1",
  "vermelho-amareladas" = "orangered1",
  "verde-amareladas" = "yellow4",
  "azul-arroxeadas" = "blueviolet",
  "roxo-azuladas" = "blueviolet",
  "lilázes" = "violet",
  "pretas" = "black",
  "negras" = "black",
  "enegrecidas" = "black",
  "marrons" = "brown",
  "amarronzadas" = "brown",
  "castanhas" = "tan1"
)
  
tokens <- tokens %>%
  mutate(cor = cores_traduzidas[segunda_palavra])
  
coordinates(tokens) = ~Longitude+Latitude
tokens <- st_as_sf(tokens, crs = 4326)
st_crs(tokens) <- 4326
br <- (filter(countries, NAME == "Brazil"))
within_brazil <- st_intersects(st_geometry(br), tokens)
tokens <- tokens[within_brazil[[1]], ]

p_colors <- tokens %>%
  ggplot() +
  geom_sf(data = br, fill = "#313131", size = 0.1, colour = "#414141") +
  geom_point(
    aes(x = longitude, y = latitude, colour = segunda_palavra),
    size = 0.8,
    alpha = 0.7
  ) +
  ggtitle("Distribuição de cores de flores em 230 mil registros de herbário ", subtitle = "Cada ponto representa uma planta e as cores se aproximam daquelas descritas nos registros") +
  scale_colour_manual(values = cores_traduzidas) +
  labs(x = NULL, y = NULL) +
  theme_plex() +
  theme(legend.position = "none") + 
  coord_sf(datum = NA)
ggsave(p_colors, file = "~/Desktop/colors.png", width = 12, height = 12)
  
  
theme_plex <- function(base_size = 14,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 18,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                          margin=margin(b=strip_text_margin),
                                          family="IBMPlexSans-Medium")
  ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                             margin=margin(b=subtitle_margin),
                                             family="IBMPlexSans")
  ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                          margin=margin(b=plot_title_margin),
                                          family="IBMPlexSans-Bold")
  ret$panel.grid.minor = ggplot2::element_blank()
  ret$panel.grid.major.x = ggplot2::element_blank()
  ret$plot.margin = ggplot2::unit(c(1,1, 1, 1), "lines")
  ret
}  
  
  
  
  
  
  
