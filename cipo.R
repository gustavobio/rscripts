# Rupestres

rupestres_shape <- st_read("~/Downloads/cr")
st_crs(rupestres_shape) <- 4326
within_rupestres <- st_intersects(st_geometry(rupestres_shape), splink_coords)
rupestres_id <- splink_coords[unlist(within_rupestres), ]$id

splink_rupestres <- 
  splink %>%
  filter(id %in% rupestres_id)

splink_rupestres_flores %>%
  ggplot() +
  geom_sf(data = rupestres_shape, fill = "white", size = 0.1) +
  theme_minimal() + 
  geom_point(aes(x = longitude, y = latitude), size = 0.0001, alpha = 0.2, col = "red") +
  labs(x = "", y = "")

cipo_shape <- st_read("~/Downloads/2-limites_APA_PN_Cipo_shape/PN Serra do Cipo.shp")
st_crs(pedreira_shape) <- 4326
pedreira_shape <- st_read("~/Downloads/2-limites_APA_PN_Cipo_shape/APA_morro_da_pedreira.shp")
st_crs(pedreira_shape) <- 4326

my_god2 <- my_god
my_god2 <- dplyr::select(my_god2, id, latitude, longitude) %>%
  mutate(Latitude = latitude, Longitude = longitude)
coordinates(my_god2) = ~Longitude+Latitude
st_crs(my_god2) <- 4326
my_god2 <- st_as_sf(my_god2, crs = 4326)

within_cipo <- st_intersects(st_geometry(cipo_shape), my_god2)
cipo_id <- my_god2[unlist(within_cipo), ]$id

within_pedreira <- st_intersects(st_geometry(pedreira_shape), splink_coords)
pedreira_id <- splink_coords[unlist(within_pedreira), ]$id

splink_cipo <- splink %>%
  filter(id %in% cipo_id)

splink_cipo %>%
  ggplot() +
  geom_sf(data = cipo_shape, fill = "white", size = 0.1) +
  theme_minimal() + 
  geom_point(aes(x = longitude, y = latitude), size = 0.01, alpha = 0.4, col = "red") +
  labs(x = "", y = "")

splink_pedreira <- splink %>%
  filter(id %in% pedreira_id)

splink_pedreira %>%
  ggplot() +
  geom_sf(data = pedreira_shape, fill = "white", size = 0.1) +
  geom_sf(data = cipo_shape, fill = "grey", size = 0.1) +
  theme_minimal() + 
  geom_point(aes(x = longitude, y = latitude), size = 0.01, alpha = 0.4, col = "red") +
  labs(x = "", y = "")

no_flower_regex <- "flor(es)?\\s?e?\\s?(frutos?)?\\s?ausentes?|est[eé]ril|sterile"
flower_regex <- "(\\b[Ii]n|\\b)[Ff]lor(?!esta|[íi]stica|amento)(ets|es|ais|al|a[cç][aã]o)?(c[eê]ncias?)?|[Ff]lower(?!ed)|[Aa]ntese|[Cc]orola|[Cc]orolla|[Ff]l\\.|[Pp][eé]talas?|[Bb]r[aá]cteas?|[Ee]st[íi]gmas?|[Aa]nteras?|[Cc][aá]lice"

splink_pedreira<- splink_pedreira %>%
  mutate(notes = tolower(notes)) %>%
  mutate(notes = gsub("\\.|,|;|\\:", " ", notes))

splink_rupestres_flores <- splink_rupestres %>%
  mutate(notes = tolower(notes)) %>%
  mutate(notes = gsub("\\.|,|;|\\:", " ", notes))

splink_pedreira_no <- splink_pedreira %>%
  filter(str_detect(notes, no_flower_regex))

splink_rupestres_no <- splink_rupestres_flores %>%
  filter(str_detect(notes, no_flower_regex))

splink_pedreira <- splink_pedreira %>%
  filter(str_detect(notes, flower_regex)) %>%
  anti_join(splink_pedreira_no)

splink_rupestres_flores <- splink_rupestres_flores %>%
  filter(str_detect(notes, flower_regex)) %>%
  anti_join(splink_rupestres_no)

splink_pedreira <- splink_pedreira %>%
  mutate(monthcollected = as.numeric(monthcollected)) %>%
  filter(monthcollected %in% 1:12) %>%
  mutate(doy = yday(ymd(paste(yearcollected, monthcollected, daycollected))))

splink_rupestres_flores <- splink_rupestres_flores %>%
  mutate(monthcollected = as.numeric(monthcollected)) %>%
  filter(monthcollected %in% 1:12) %>%
  mutate(doy = yday(ymd(paste(yearcollected, monthcollected, daycollected))))

splink_pedreira %>%
  ggplot(aes(x = doy, y = family_apg)) + 
  geom_point()

test1 <-splink_rupestres_flores_cf %>%
  left_join(flores_spp, by = c("scientificname" = "original.search"))

test2 <- test1 %>%
  mutate(family_fixed = coalesce(family.x, family.y)) %>%
  mutate(species_fixed = coalesce(search.str, scientificname)) %>%
  mutate(status_fixed = coalesce(taxon.status, taxon_status)) %>%
  mutate(rank_fixed = coalesce(taxon.rank, taxon_rank)) %>%
  mutate(full_name = coalesce(scientific.name, scientific_name)) %>%
  mutate(threat = coalesce(threat.status, threat_status)) %>%
  select(full_name, species_fixed, family_fixed, rank_fixed, threat, institutioncode, collectioncode, catalognumber, 
         identifiedby, yearidentified, monthidentified, dayidentified, yearcollected, monthcollected, 
         daycollected, country, stateprovince, county, locality, longitude, latitude, longitude_mun, latitude_mun, notes.x, barcode, id.x)


rupestres <- read.csv2("cipo/rupestres.csv", colClasses = c("character", "character", "character", rep("NULL", 29), "integer", rep("NULL", 8)))

splink_rup <- filter(splink, id %in% rupestres$id)

rup_names <- data.frame(original_name = splink_rup$scientificname)
rup_names <- filter(rup_names, !grepl("[Cc][Ff]\\.", rup_names$original_name))
rup_names <- filter(rup_names, !grepl("\\([Cc][Ff]\\)", rup_names$original_name))
rup_names <- filter(rup_names, !grepl("[Aa]ff\\.", rup_names$original_name))


rup_names$new_name <- rup_names$original_name
rup_names$new_name <- trimws(gsub("\\ssp\\.", "", rup_names$original_name))
rup_names <- filter(rup_names, !grepl("^$", rup_names$new_name))
rup_names <- filter(rup_names, !grepl("[Ii]ndet.", rup_names$new_name))


rup_names_fixed <- list()
for (i in unique(rup_names$new_name)) {
  print(i)
  res <- get.taxa(i)  
  if (grepl("not found", res$notes)) {
    print("Entrou")
    res <- get.taxa(i, parse = TRUE)
    if (grepl("not found", res$notes)) {
      split_name <- strsplit(i, " ")[[1]]
      if (length(split_name) >= 3) {
        res <- get.taxa(paste(split_name[1:2], collapse = " "))
        res$original.search <- i
      }
    }
  }
  rup_names_fixed[[i]] <- res
}

rup <- do.call("rbind.data.frame", rup_names_fixed)

my_god <- left_join(splink_rup, rup, by = c("scientificname" = "original.search"))

my_god <- my_god %>%
  dplyr::select(-search_str, -threat_status, -accepted_name, -taxon_rank, -taxon_status, -name_notes, -scientific_name) %>%
  plyr::rename(replace = c("notes.x" = "collector_notes", "id.x" = "id", "id.y" = "id_pm", "family.y" = "family_pm", "notes.y" = "notes_pm"))

my_god$cipo <- FALSE
my_god$cipo[my_god$id %in% cipo_id] <- TRUE

my_god %>%
  ggplot() +
 geom_sf(data = rupestres_shape, fill = "white", size = 0.1) +
  geom_sf(data = cipo_shape, fill = "white", size = 0.1) + 
  theme_minimal() + 
  geom_point(aes(x = longitude, y = latitude), size = 0.0001, alpha = 0.2, col = "red") +
  labs(x = "", y = "")

my_god %>%
  filter(cipo == TRUE) %>%
  ggplot() +
  geom_histogram(aes(x = doy))
