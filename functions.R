load_splink_data <-
  function(file = "speciesLink_all_8566_20170802154748.txt") {
    splink <- read_delim(
      file,
      col_types = cols_only(
        scientificname = col_character(),
        institutioncode = col_character(),
        collectioncode = col_character(),
        catalognumber = col_character(),
        collector = col_character(),
        collectornumber = col_character(),
        notes = col_character(),
        family = col_character(),
        genus = col_character(),
        species = col_character(),
        subspecies = col_character(),
        identifiedby = col_character(),
        dayidentified = col_character(),
        monthidentified = col_character(),
        yearidentified = col_character(),
        country = col_character(),
        stateprovince = col_character(),
        county = col_character(),
        country = col_character(),
        locality = col_character(),
        notes = col_character(),
        barcode = col_character(),
        yearcollected = col_character(),
        monthcollected = col_character(),
        daycollected = col_character(),
        stateprovince = col_character(),
        longitude = col_character(),
        latitude = col_character(),
        longitude_mun = col_character(),
        latitude_mun = col_character()
      ),
      comment = "",
      quoted_na = FALSE,
      delim = "\t",
      quote = "\"",
      escape_double = FALSE,
      na = "NA"
    )
    splink
  }

load_rb_data <- function() {
  rb <- search_rb("")
  names(rb) <- tolower(names(rb))
  rb <- rb %>%
    dplyr::select(
      catalognumber,
      collectioncode,
      institutioncode,
      country,
      county = municipality,
      daycollected = day,
      monthcollected = month,
      yearcollected = year,
      collector = recordedby,
      latitude = decimallatitude,
      longitude = decimallongitude,
      family,
      genus,
      species = specificepithet,
      subspecies = infraspecificepithet,
      scientificname,
      scientificnameauthorship,
      collectornumber = recordnumber,
      notes = fieldnotes,
      identifiedby,
      associatedmedia,
      locality,
      taxonrank
    ) %>%
    mutate(
      scientificname = case_when(
        taxonrank %in% c("ssp.", "var.", "Infr.") ~ paste(genus, species, taxonrank, subspecies),
        taxonrank == "family" ~ family,
        taxonrank == "genus" ~ genus,
        taxonrank == "specie" ~ paste(genus, species),
        TRUE ~ paste(genus, species)
      )
    ) %>%
    mutate(yearcollected = as.character(yearcollected)) %>%
    mutate(monthcollected = as.character(monthcollected)) %>%
    mutate(daycollected = as.character(daycollected))
  rb
}

load_kew_data <- function() {
  kew <- kew %>%
    filter(countryCode == "BR")
  names(kew) <- tolower(names(kew))
  kew <- kew %>%
    mutate(eventdate = ymd(eventdate)) %>%
    mutate(daycollected = day(eventdate)) %>%
    mutate(monthcollected = month(eventdate)) %>%
    mutate(yearcollected = year(eventdate))
  kew <- kew %>%
    dplyr::select(
      catalognumber,
      collectioncode,
      country,
      institutioncode,
      county = municipality,
      daycollected,
      monthcollected,
      yearcollected,
      collector = recordedby,
      latitude = decimallatitude,
      longitude = decimallongitude,
      family,
      genus,
      species = specificepithet,
      subspecies = infraspecificepithet,
      scientificname,
      scientificnameauthorship,
      collectornumber = recordnumber,
      notes = eventremarks,
      identifiedby,
      locality,
      taxonrank
    ) %>%
    mutate(
      scientificname = case_when(
        taxonrank %in% c("ssp.", "var.", "Infr.") ~ paste(genus, species, taxonrank, subspecies),
        taxonrank == "family" ~ family,
        taxonrank == "genus" ~ genus,
        taxonrank == "specie" ~ paste(genus, species),
        TRUE ~ paste(genus, species)
      )
    ) %>%
    mutate(yearcollected = as.character(yearcollected)) %>%
    mutate(monthcollected = as.character(monthcollected)) %>%
    mutate(daycollected = as.character(daycollected))
  kew
}


to_circular <- function(x) {
  suppressWarnings(as.circular(
    x * (360 / 366),
    units = "degrees",
    rotation = "clock",
    modulo = "2pi"
  ))
}
