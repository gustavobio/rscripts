require(tidyverse)
library(rgeos)
library(sp)
library(rgdal)
library(sf)
library(readr)
library(readxl)
library(lubridate)
library(RB)
require(stringr)
source("functions.R")
# Load all splink specimens
splink <- load_splink_data()

# Get all data from RB (JBRJ)
rb <- load_rb_data()

# Kew
kew <- load_kew_data()
# NYBG records with notes downloaded. This had to be pre-processed
load("nybg.rda")

# Unique species processed through plantminer
load("spp_processed.RData")

spp <- spp %>%
  mutate(name_notes = notes) %>%
  mutate(family_apg = family) %>%
  dplyr::select(-notes, -family) %>%
  distinct()

names(spp) <- gsub("\\.", "_", names(spp))

# Select names in nybg that occurr in splink
nybg <- nybg %>%
  mutate(notes = description) %>% 
  mutate_if(is.numeric, as.character) %>%
  dplyr::select(names(splink)) %>%
  mutate(scientificname = str_replace_all(scientificname, "[^A-Za-z0-9-\\s\\.]", ""))

# splink wrangling
splink$longitude[splink$longitude_mun != ""] <- splink$longitude_mun[splink$longitude_mun != ""]

splink$latitude[splink$latitude_mun != ""] <- splink$latitude_mun[splink$latitude_mun != ""]

splink <- splink %>%
  filter(!longitude %in% c("", "0") | !longitude_mun %in% c("", "0")) %>%
  filter(latitude != "Bloqueada") %>%
  filter(scientificname != "") %>%
  filter(yearcollected != "") %>%
  filter(monthcollected != "") %>%
  filter(!daycollected %in% c("", "00")) %>%
  anti_join(nybg, by = c("barcode", "catalognumber", "institutioncode")) %>%
  bind_rows(inner_join(dplyr::select(splink, barcode, catalognumber, institutioncode), nybg)) %>%
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
  full_join(rb) %>%
  filter(!is.na(latitude), !is.na(longitude))

kew <- kew %>%
  mutate(latitude = as.numeric(latitude)) %>%
  mutate(longitude = as.numeric(longitude))

splink <- splink %>%
  full_join(kew) %>%
  filter(!is.na(latitude), !is.na(longitude))

# Creating doys
splink <- splink %>%
  mutate(doy = yday(dmy(paste(daycollected, monthcollected, yearcollected, sep = "/"))))

# Fixing years
splink <- splink %>%
  mutate(yearcollected = as.numeric(yearcollected)) %>%
  filter(between(yearcollected, 1814, 2017))

# Giving each entry a unique ID
splink <- splink %>%
  mutate(id = 1:NROW(splink))

# Creating decades
decades <- seq(1810, 2020, by = 10)
splink$decade <- cut(splink$yearcollected, decades, labels = decades[-length(decades)], right = FALSE)

# Shapes needed for mapping
countries <- st_read("~/Downloads/ne_10m_admin_0_countries/")
#rodovias <- st_read("Rodovias_shp/")
#br_municipios <- st_read("br_municipios/")
st_crs(br_municipios) <- 4326
br_municipios$n_spp <- unlist(lapply(st_contains(br_municipios, splink), length))
br <- filter(countries, NAME == "Brazil")
municipios <- read_excel("anexo_16261_Coordenadas_Sedes_5565_Municípios_2010.xls")
municipios_pop <- read.csv2("municipios_2015.csv", dec = ",")
names(municipios) <- tolower(names(municipios))
names(municipios_pop) <- c("UF", "cod_UF", "cod_municipio", "nome_municipio", "populacao")
ecoregions <- st_read("~/Downloads/terr-ecoregions-TNC/")
ecoregions_br <- st_intersection(br, ecoregions)
ecoregions_br <- ecoregions_br %>%
  mutate(ECO_NAME = case_when(
    ECO_NAME == "Caqueta Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Guianan Highlands Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Guianan Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Guianan Piedmont And Lowland Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Japurá-Solimoes-Negro Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Madeira-Tapajós Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Negro-Branco Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Purus-Madeira Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Solimoes-Japurá Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Southwest Amazon Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Tapajós-Xingu Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Tocantins/Pindare Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Uatuma-Trombetas Moist Forests" ~ "Amazon Moist Forests",
    ECO_NAME == "Xingu-Tocantins-Araguaia Moist Forests" ~ "Amazon Moist Forests", 
    ECO_NAME == "Bahia Coastal Forests" ~ "Bahia Forests",
    ECO_NAME == "Bahia Interior Forests" ~ "Bahia Forests",
    TRUE ~ as.character(ECO_NAME)
  ))

municipios_pop <- municipios_pop %>%
  mutate(cod_municipio = sprintf("%05s", cod_municipio)) %>%
  unite(geocodigo_municipio, cod_UF, cod_municipio, sep = "")

municipios <- municipios %>%
  dplyr::select(-nome_municipio) %>%
  left_join(municipios_pop, by = "geocodigo_municipio") %>%
  mutate(populacao = as.numeric(gsub("\\D", "", populacao)))

# Just seems to be a bit faster than converting directly to a geometry collection

splink_coords <- dplyr::select(splink, id, latitude, longitude) %>%
  mutate(Latitude = latitude, Longitude = longitude)
coordinates(splink_coords) = ~Longitude+Latitude
splink_coords <- st_as_sf(splink_coords, crs = 4326)
st_crs(splink_coords) <- 4326
within_brazil <- st_intersects(st_geometry(br), splink_coords)
splink_coords <- splink_coords[within_brazil[[1]], ]

splink <- 
  splink %>%
  filter(id %in% splink_coords$id) %>%
  left_join(dplyr::select(spp, -id), by = c("scientificname" = "original_search"))

no_flower_regex <- "flor(es)?\\s?e?\\s?(frutos?)?\\s?ausentes?|est[eé]ril|sterile"
flower_regex <- "(\\b[Ii]n|\\b)[Ff]lor(?!esta|[íi]stica|amento)(ets|es|ais|al|a[cç][aã]o)?(c[eê]ncias?)?|[Ff]lower(?!ed)|[Aa]ntese|[Cc]orola|[Cc]orolla|[Ff]l\\.|[Pp][eé]talas?|[Bb]r[aá]cteas?|[Ee]st[íi]gmas?|[Aa]nteras?|[Cc][aá]lice"

splink <- splink %>%
  mutate(notes = tolower(notes)) %>%
  mutate(notes = gsub("\\.|,|;|\\:", " ", notes))

splink_no_flowers <- splink %>%
  filter(str_detect(notes, no_flower_regex))

splink <- splink %>%
  filter(str_detect(notes, flower_regex)) %>%
  anti_join(splink_no_flowers)

 splink <- splink %>%
  mutate(monthcollected = as.numeric(monthcollected)) %>%
  filter(monthcollected %in% 1:12) %>%
  mutate(doy = yday(ymd(paste(yearcollected, monthcollected, daycollected))))
 
 splink %>%
   dplyr::select(-id) %>%
   distinct()
   
 
 
