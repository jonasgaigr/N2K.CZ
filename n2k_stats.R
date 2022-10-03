setwd("C:/Users/jonas.gaigr/N2K.CZ")
# LOAD PACKAGES ----
if(!isTRUE(require(devtools, quietly = TRUE))) {
  install.packages("devtools", dependencies = TRUE); library(devtools)
} else {
  require(devtools)}

if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

# LOAD DATA ----
sites_subjects <- openxlsx::read.xlsx("http://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000.xlsx")
sites_subjects <- sites_subjects %>%
  dplyr::rename(Nazev.latinsky = "Název.latinsky.(druh)",
                Nazev.lokality = "Název.lokality",
                Kod.lokality = "Kód.lokality",
                feature_code = "Kód.fenoménu")
sites_subjects$Nazev.latinsky <- gsub("Osmoderma eremita", "Osmoderma barnabita", sites_subjects$Nazev.latinsky)
sites_subjects$Nazev.latinsky <- gsub("Stenobothrus eurasius bohemicus", "Stenobothrus eurasius", sites_subjects$Nazev.latinsky)
# Druhové seznamy Přílohy II
taxa <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/taxa.csv", encoding = "UTF-8")

species_read <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_data_export_2022_03_encoded.csv",
                         sep = ",",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")

species <- species_read %>% 
  mutate(
    # Převedení druhu na kategorickou veličinu
    DRUH = as.factor(DRUH),
    # Převedení datumu do vhodného formátu
    DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na rok
    YEAR = substring(DATE, 1, 4),
    # Izolace kódu EVL
    SITECODE = substr(EVL, 1, 9),
    # Izolace názvu lokality
    NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL)))
  )

evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl <- st_transform(evl, CRS("+init=epsg:4326"))
mammal_evl <- st_read("Biotop_zvl%C3%A1%C5%A1t%C4%9B_chr%C3%A1n%C4%9Bn%C3%BDch_druh%C5%AF_velk%C3%BDch_savc%C5%AF.shp")
mammal_evl <- st_transform(mammal_evl, CRS("+init=epsg:4326"))
czechia <- st_read("HraniceCR.shp")
czechia <- st_transform(czechia, CRS("+init=epsg:4326"))
bioregs <- st_read("BiogeoRegions_CR.shp")
bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))

# FUNKCE N2K ----
# SITECODE EVL PODLE PŘEDMĚTU OCHRANY
find_evl_SITECODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.česky == species) %>%
           dplyr::pull(Kód.lokality) %>%
           unique()
  )
}
# NÁZEV EVL PODLE PŘEDMĚTU OCHRANY
find_evl_NAZEV <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Nazev.latinsky == species) %>%
           dplyr::pull(Nazev.lokality) %>%
           unique()
  )
}
# POČET EVL PODLE PŘEDMĚTU OCHRANY
find_evl_NUMBER <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Nazev.latinsky == species) %>%
           nrow()
  )
}
# SITECODE EVL PODLE NÁZVU
find_evl_NAME_TO_CODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.lokality == species) %>%
           dplyr::pull(Kód.lokality) %>%
           unique()
  )
}
# BIOTOPY EVD
find_evl_HAB_QUAL <- function(species) {
  return(phengaris_habitaty %>%
           filter(SITECODE == species) %>%
           pull(HAB_AREA)
  )
}

# SEZNAM PŘEDMĚTŮ OCHRANY EVL
find_evl_TARGETS <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Kod.lokality == species) %>%
           dplyr::pull(Název.česky)
  )
}
# NÁZEV EVL PODLE KÓDU
find_evl_CODE_TO_NAME <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Kod.lokality == species) %>%
           dplyr::pull(Nazev.lokality) %>%
           unique()
  )
}

find_N2K_feature_code <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Nazev.latinsky == species) %>%
           dplyr::pull(feature_code) %>%
           unique()
  )
}

find_N2K_name <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(feature_code == species) %>%
           dplyr::pull(Nazev.latinsky) %>%
           unique()
  )
}

# STATS ----
stats_2021 <- species %>%
  filter(YEAR == 2021) %>%
  group_by(DRUH) %>%
  filter(NAZEV  %in% find_evl_NAZEV(unique(DRUH))) %>%
  summarise(EVL_COUNT = length(unique(EVL)),
            EVL_PERC = EVL_COUNT/find_evl_NUMBER(unique(DRUH))*100,
            NAZEV_LIST = toString(unique(NAZEV)),
            SITECODE_LIST = toString(unique(SITECODE)),
            FIND_COUNT = length(unique(IDX_ND_AKCE))) %>%
  arrange(-FIND_COUNT)

stats_2021
write.csv2(stats_2021,
           "C:/Users/jonas.gaigr/Desktop/stats_2021.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

target_lep_2020 <- species %>%
  filter(YEAR == 2020) %>%
  filter(ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
         ZDROJ == "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR." |
         ZDROJ == "Sledování stavu EVL - IPLife") %>%
  group_by(DRUH) %>%
  filter(NAZEV  %in% find_evl_NAZEV(unique(DRUH))) %>%
  summarise(EVL_COUNT = length(unique(EVL)),
            EVL_PERC = EVL_COUNT/find_evl_NUMBER(unique(DRUH))*100,
            NAZEV_LIST = toString(unique(NAZEV)),
            SITECODE_LIST = toString(unique(SITECODE)),
            FIND_COUNT = length(unique(IDX_ND_AKCE))) %>%
  arrange(-FIND_COUNT)
species %>%
  filter(DRUH == "Phengaris nausithous") %>%
  filter(YEAR == 2021) %>%
  pull(ZDROJ) %>%
  unique
