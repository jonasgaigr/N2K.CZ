# LOAD PACKAGES ----
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
if(!isTRUE(require(xlsx, quietly = TRUE))) {
  install.packages("xlsx", dependencies = TRUE); library(xlsx)
} else {
  require(xlsx)}

# LOAD DATA ----
sites_subjects <- openxlsx::read.xlsx("http://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000_440_2021.xlsx",
                                      sheet = 1) %>%
write.csv(sites_subjects, 
          "sites_subjects_utf-8.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

sites_subjects <- sites_subjects %>%
  dplyr::rename(Nazev.latinsky = "Název.latinsky.(druh)",
                Nazev.lokality = "Název.lokality",
                Kod.lokality = "Kód.lokality",
                feature_code = "Kód.fenoménu")
sites_subjects$Nazev.latinsky <- gsub("Osmoderma eremita", "Osmoderma barnabita", sites_subjects$Nazev.latinsky)
sites_subjects$Nazev.latinsky <- gsub("Stenobothrus eurasius bohemicus", "Stenobothrus eurasius", sites_subjects$Nazev.latinsky)
str(sites_subjects)
xxx <- "Bzenec"
Encoding(sites_subjects$Kod.lokality) <- "Windows-1250"
# Druhové seznamy Přílohy II
taxa <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/taxa.csv", encoding = "UTF-8")

species_read <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_data_export_2022_08_encoded.csv",
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

phengaris_habitaty <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/phengaris_habitaty.csv", encoding = "UTF-8")

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
           dplyr::filter(Název.česky == species) %>%
           dplyr::pull(Název.lokality) %>%
           unique()
  )
}
# POČET EVL PODLE PŘEDMĚTU OCHRANY
find_evl_NUMBER <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(feature_code == species) %>%
           nrow()
  )
}
# SITECODE EVL PODLE NÁZVU
find_evl_NAME_TO_CODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Nazev.lokality == species) %>%
           dplyr::pull(Kod.lokality) %>%
           unique()
  )
}
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

# FUNKCE LEP ----
ins_lep_1_quad_eval <- function(species_code, evl_site) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(PRESNOST < 500) %>%
    filter(SITECODE == evl_site) %>%
    filter(DRUH == species_code) %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      MOWING_TIME_find = dplyr::case_when(grepl("<sec_nacasovani>ne", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                          grepl("<sec_nacasovani>ano", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      MOWING_METHOD_find = dplyr::case_when(grepl("<sec_celoplosna>ano", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                            grepl("<sec_celoplosna>ne", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      PLANTS_val = readr::parse_character(gsub(".*<STA_PRITOMNOSTROSTLIN>|</STA_PRITOMNOSTROSTLIN>.*", "", STRUKT_POZN)),
      PLANTS_find = dplyr::case_when(grepl("<STA_PRITOMNOSTROSTLIN>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<STA_PRITOMNOSTROSTLIN>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<STA_PRITOMNOSTROSTLIN>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<STA_PRITOMNOSTROSTLIN>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1,),
      TARGET_MON_find = dplyr::case_when(ZDROJ == "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Sledování stavu EVL - IPLife" ~ 1,
                                         TRUE ~ 0)
      ) %>%
  dplyr::mutate(
    MANAGEMENT_find = dplyr::case_when(MOWING_TIME_find == 1 &
                                         MOWING_METHOD_find == 1 ~ 1,
                                       MOWING_TIME_find == 0 |
                                         MOWING_METHOD_find == 0 ~ 0),
    DESTRUCT_find = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                   "odvodňování, meliorace",
                                                   "zalesňování bezlesí", 
                                                   "změna zemědělského využívání půdy"), 
                                                 collapse = "|"), 
                                           STRUKT_POZN, 
                                           ignore.case = TRUE) ~ 0,
                                     TARGET_MON_find == 1 ~ 1),
    THREATS_val = readr::parse_character(gsub(".*<vliv>|</vliv>.*", "", STRUKT_POZN)),
    THREATS_find = dplyr::case_when(grepl("žádný", STRUKT_POZN) ~ 1,
                                    grepl("<vliv>", STRUKT_POZN) &
                                      TARGET_MON_find == 1 ~ 0,
                                    TARGET_MON_find == 1 ~ 1),
    THREATS_NUM_find = stringr::str_count(STRUKT_POZN, ",")
  ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         PLANTS_find,
                         MANAGEMENT_find,
                         DESTRUCT_find,
                         THREATS_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_sites <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::arrange(desc(TARGET_MON_find),
                   desc(YEAR), 
                   desc(OVERALL_find), 
                   desc(DATE)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(
      PRESENCE_site = unique(PRESENCE_find),
      PLANTS_site = unique(PLANTS_find),
      MANAGEMENT_site = unique(MANAGEMENT_find),
      DESTRUCT_site = unique(DESTRUCT_find),
      THREATS_val = unique(THREATS_val),
      THREATS_site = unique(THREATS_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find),
      DATE = unique(DATE)
           ) %>%
    dplyr::ungroup()
  
  species_target_site_ID <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::arrange(desc(TARGET_MON_find),
                   desc(YEAR), 
                   desc(OVERALL_find), 
                   desc(DATE)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(SITECODE = evl_site,
                     ID_ND_NALEZ = unique(ID_ND_NALEZ)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, POLE, ID_ND_NALEZ)
  
  last_targeted_mon_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::summarise(SITECODE = evl_site,
                     LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, POLE, LAST_TARGET_MON)
  
  last_find_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(SITECODE = evl_site,
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, POLE, LAST_FIND)
  
  species_target_quad <- species_target_sites %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::group_by(POLE) %>%
    dplyr::summarise(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = species_code,
      FEATURE_CODE = find_N2K_feature_code(species_code),
      POLE = unique(POLE),
      PRESENCE = dplyr::case_when(max(na.omit(PRESENCE_site)) == 1 ~ 1,
                                  max(na.omit(PRESENCE_site)) == 0 ~ 0),
      ROSTLINY = mean(na.omit(PLANTS_site)),
      LIKVIDACE = mean(na.omit(DESTRUCT_site)),
      NEGATIV_LIST = dplyr::case_when(max(TARGET_MON_site) == 0 ~ NA_character_,
                                      mean(na.omit(THREATS_site)) == 0 ~ unique(THREATS_val),
                                      TRUE ~ "žádné"),
      NEGATIV = mean(na.omit(THREATS_site)),
      MANAGEMENT = mean(na.omit(MANAGEMENT_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site)),
      SUFFICIENT = dplyr::case_when(PRESENCE == 0 ~ 0,
                                    OVERALL == 5 ~ 1,
                                    OVERALL < 5 & PRESENCE > 0 ~ 0.5),
      EVAL = dplyr::case_when(is.na(sum(PRESENCE, 
                                        ROSTLINY,
                                        LIKVIDACE,
                                        MANAGEMENT,
                                        NEGATIV)) == TRUE ~ 0,
                                    TRUE ~ 1)
      ) %>%
    dplyr::ungroup()
  
  result <- species_target_quad %>%
    dplyr::full_join(.,
                     species_target_site_ID,
                     by = c("SITECODE", "POLE")) %>%
    dplyr::full_join(.,
                     last_targeted_mon_quad, 
                     by = c("SITECODE", "POLE")) %>%
    dplyr::full_join(., 
                      last_find_quad, 
                      by = c("SITECODE", "POLE"))
  
  result
  
}

ins_lep_1_sci_eval <- function(species_code, sci_code) {
  
  # SCI LEVEL
  species_target_sci <- results_quad_lep_1 %>%
    dplyr::filter(FEATURE_CODE == species_code)  %>%
    dplyr::filter(SITECODE == sci_code)
  
  if(nrow(species_target_sci) > 0) {
    result <- species_target_sci %>%
      dplyr::group_by(SITECODE) %>%
      dplyr::summarise(
        NAZEV = unique(NAZEV),
        FEATURE_CODE = unique(FEATURE_CODE),
        DRUH = unique(DRUH),
        PRESENCE = mean(na.omit(PRESENCE)),
        ROSTLINY = mean(na.omit(ROSTLINY)),
        LIKVIDACE = mean(na.omit(LIKVIDACE)),
        NEGATIV = mean(na.omit(NEGATIV)),
        MANAGEMENT = mean(na.omit(MANAGEMENT)),
        HABITAT_PERC = dplyr::case_when(identical(find_evl_HAB_QUAL(sci_code), numeric(0)) == TRUE ~ 0,
                                        TRUE ~ find_evl_HAB_QUAL(sci_code)),
        HABITAT = dplyr::case_when(HABITAT_PERC == 0 ~ 0,
                                   length(find_evl_TARGETS(sci_code)) == 1 &
                                     find_evl_HAB_QUAL(sci_code) < 0.5 ~ 0,
                                   length(find_evl_TARGETS(sci_code)) == 1 &
                                     find_evl_HAB_QUAL(sci_code) >= 0.5 ~ 1,
                                   length(find_evl_TARGETS(sci_code)) > 1 &
                                     find_evl_HAB_QUAL(sci_code) < 0.1 ~ 0,
                                   length(find_evl_TARGETS(sci_code)) > 1 &
                                     find_evl_HAB_QUAL(sci_code) >= 0.1 ~ 1),
        OVERALL = mean(na.omit(OVERALL)),
        SUFFICIENT = dplyr::case_when(PRESENCE == 0 ~ 0,
                                      PRESENCE >= 0.5 &
                                        ROSTLINY >= 0.5 &
                                        LIKVIDACE >= 0.5 &
                                        NEGATIV >= 0.5 &
                                        MANAGEMENT >= 0.5 &
                                        HABITAT >= 0.5 ~ 1,
                                      PRESENCE < 0.5 |
                                        ROSTLINY < 0.5 |
                                        LIKVIDACE < 0.5 |
                                        NEGATIV < 0.5 |
                                        MANAGEMENT < 0.5 |
                                        HABITAT < 0.5 ~ 0.5),
        TARGET_MON = max(na.omit(LAST_TARGET_MON)))
  } else {
    result <- tibble(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      FEATURE_CODE = species_code,
      DRUH = find_N2K_name(species_code),
      PRESENCE = NA,
      ROSTLINY = NA,
      LIKVIDACE = NA,
      NEGATIV = NA,
      MANAGEMENT = NA,
      HABITAT_PERC = dplyr::case_when(identical(find_evl_HAB_QUAL(sci_code), numeric(0)) == TRUE ~ 0,
                                      TRUE ~ find_evl_HAB_QUAL(sci_code)),
      HABITAT = dplyr::case_when(HABITAT_PERC == 0 ~ 0,
                                 find_evl_TARGETS(sci_code) == 1 &
                                   find_evl_HAB_QUAL(sci_code) < 0.5 ~ 0,
                                 find_evl_TARGETS(sci_code) == 1 &
                                   find_evl_HAB_QUAL(sci_code) >= 0.5 ~ 1,
                                 find_evl_TARGETS(sci_code) == 0 &
                                   find_evl_HAB_QUAL(sci_code) < 0.1 ~ 0,
                                 find_evl_TARGETS(sci_code) == 0 &
                                   find_evl_HAB_QUAL(sci_code) >= 0.1 ~ 1),
      TARGET_MON = NA,
      OVERALL = NA,
      SUFFICIENT = NA)
  }
  
  result <- result %>%
    distinct()

  result
  
}

ins_lep_2_quad_eval <- function(species_code, evl_site) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(PRESNOST < 500) %>%
    filter(SITECODE == evl_site) %>%
    filter(DRUH == species_code) %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      PLANTS_find = dplyr::case_when(grepl("<STA_PRITOMNOSTROSTLIN>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<STA_PRITOMNOSTROSTLIN>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<STA_PRITOMNOSTROSTLIN>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<STA_PRITOMNOSTROSTLIN>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(ZDROJ == "Kolektiv autorů (2020) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring motýlů. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Sledování stavu EVL - IPLife" ~ 1,
                                         TRUE ~ 0)
    ) %>%
    dplyr::mutate(
      DESTRUCT_find = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                     "odvodňování, meliorace",
                                                     "zalesňování bezlesí", 
                                                     "změna zemědělského využívání půdy"), 
                                                   collapse = "|"), 
                                             STRUKT_POZN, 
                                             ignore.case = TRUE) ~ 0,
                                       TARGET_MON_find == 1 ~ 1),
      THREATS_find = dplyr::case_when(grepl("žádný vliv", STRUKT_POZN) ~ 1,
                                      TARGET_MON_find == 1 ~ 0),
      THREATS_NUM_find = stringr::str_count(STRUKT_POZN, ",")
    ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         PLANTS_find,
                         DESTRUCT_find,
                         THREATS_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_sites <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::arrange(desc(OVERALL_find), desc(DATE)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(
      PRESENCE_site = unique(PRESENCE_find),
      PLANTS_site = unique(PLANTS_find),
      DESTRUCT_site = unique(DESTRUCT_find),
      THREATS_site = unique(THREATS_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find),
      DATE = unique(DATE)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::summarise(SITECODE = evl_site,
                     LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, POLE, LAST_TARGET_MON)
  
  last_find_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(SITECODE = evl_site,
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, POLE, LAST_FIND)
  
  species_target_quad <- species_target_sites %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::group_by(POLE) %>%
    dplyr::summarise(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = species_code,
      FEATURE_CODE = find_N2K_feature_code(species_code),
      POLE = unique(POLE),
      PRESENCE = dplyr::case_when(max(na.omit(PRESENCE_site)) == 1 ~ 1,
                                  max(na.omit(PRESENCE_site)) == 0 ~ 0),
      ROSTLINY = mean(na.omit(PLANTS_site)),
      LIKVIDACE = mean(na.omit(DESTRUCT_site)),
      NEGATIV = mean(na.omit(THREATS_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site)),
      SUFFICIENT = dplyr::case_when(PRESENCE == 0 ~ 0,
                                    OVERALL == 4 ~ 1,
                                    OVERALL < 4 & PRESENCE > 0 ~ 0.5),
      EVAL = dplyr::case_when(is.na(sum(PRESENCE, 
                                        ROSTLINY,
                                        LIKVIDACE,
                                        NEGATIV)) == TRUE ~ 0,
                              TRUE ~ 1)) %>%
    dplyr::ungroup()
  
  result <- species_target_quad %>%
    dplyr::full_join(.,
                     last_targeted_mon_quad, 
                     by = c("SITECODE", "POLE")) %>%
    dplyr::full_join(., 
                     last_find_quad, 
                     by = c("SITECODE", "POLE"))
  
  result
  
}

ins_lep_2_sci_eval <- function(species_code, sci_code) {
  
  # SCI LEVEL
  species_target_sci <- results_quad_lep_2 %>%
    dplyr::filter(FEATURE_CODE == species_code)  %>%
    dplyr::filter(SITECODE == sci_code)
  
  if(nrow(species_target_sci) > 0) {
    result <- species_target_sci %>%
      dplyr::summarise(
        SITECODE = unique(SITECODE),
        NAZEV = unique(NAZEV),
        FEATURE_CODE = unique(FEATURE_CODE),
        DRUH = unique(DRUH),
        PRESENCE = mean(na.omit(PRESENCE)),
        ROSTLINY = mean(na.omit(ROSTLINY)),
        LIKVIDACE = mean(na.omit(LIKVIDACE)),
        NEGATIV = mean(na.omit(NEGATIV)),
        TARGET_MON = max(na.omit(LAST_TARGET_MON)),
        OVERALL = mean(na.omit(OVERALL)),
        SUFFICIENT = mean(na.omit(SUFFICIENT)))
  } else {
    result <- tibble(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      FEATURE_CODE = species_code,
      DRUH = find_N2K_name(species_code),
      PRESENCE = NA,
      ROSTLINY = NA,
      LIKVIDACE = NA,
      NEGATIV = NA,
      TARGET_MON = NA,
      OVERALL = NA,
      SUFFICIENT = NA)
  }
  
  result
  
}

# RESULTS LEP ----
# |-Phengaris nausithous ----
sites_phenau <- sites_subjects %>%
  filter(feature_code == 6179)

hu_phenau <- ins_lep_1_quad_eval(sites_phenau[1,7], sites_phenau[1,1])
results_quad_phenau <- matrix(NA, 1, ncol(hu_phenau)) %>% 
  dplyr::as_tibble()
colnames(results_quad_phenau) <- colnames(hu_phenau)

for(i in 1:nrow(sites_phenau)) {
  results_quad_phenau <- dplyr::bind_rows(results_quad_phenau, 
                                          as.data.frame(ins_lep_1_quad_eval("Phengaris nausithous", sites_phenau[i,1])))
}

results_quad_phenau <- results_quad_phenau[c(2:nrow(results_quad_phenau)),]

write.csv(results_quad_phenau, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_phenau_quad.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

# |-Phengaris teleius ----
sites_phetel <- sites_subjects %>%
  filter(feature_code == 6177)
hu_phetel <- ins_lep_1_quad_eval(sites_phetel[1,7], sites_phetel[1,1])
results_quad_phetel <- matrix(NA, 1, ncol(hu_phetel)) %>% 
  dplyr::as_tibble()
colnames(results_quad_phetel) <- colnames(hu_phetel)

for(i in 1:nrow(sites_phetel)) {
  results_quad_phetel <- dplyr::bind_rows(results_quad_phetel, 
                                          as.data.frame(ins_lep_1_quad_eval(sites_phetel[1,7], sites_phetel[i,1])))
}

results_quad_phetel <- results_quad_phetel[c(2:nrow(results_quad_phetel)),]

write.csv(results_quad_phetel, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_phetel_quad.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

#|-lep_1_quad_results ----
results_quad_lep_1 <- dplyr::bind_rows(results_quad_phenau,
                                       results_quad_phetel)
#|-lep_1_sci_results ----
sites_lep_1 <- sites_subjects %>%
  dplyr::filter(feature_code == 6177 | feature_code == 6179)
hu_lep_1 <- ins_lep_1_sci_eval(sites_lep_1[20,5],
                               sites_lep_1[20,1])
results_sci_lep_1 <- matrix(NA, 1, ncol(hu_lep_1)) %>% 
  dplyr::as_tibble()
colnames(results_sci_lep_1) <- colnames(hu_lep_1)

for(i in 1:nrow(sites_lep_1)) {
  results_sci_lep_1 <- dplyr::bind_rows(results_sci_lep_1, 
                                          as.data.frame(ins_lep_1_sci_eval(sites_lep_1[i,5], sites_lep_1[i,1])))
}
results_sci_lep_1 <- results_sci_lep_1[c(2:nrow(results_sci_lep_1)),]

write.csv(results_sci_lep_1, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_lep_1_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

# |-Euplagia quadripunctaria ----
sites_eupqua <- sites_subjects %>%
  filter(feature_code == 6199)
hu_eupqua <- ins_lep_2_quad_eval(sites_eupqua[1,7], sites_eupqua[1,1])
results_quad_eupqua <- matrix(NA, 1, ncol(hu_eupqua)) %>% 
  dplyr::as_tibble()
colnames(results_quad_eupqua) <- colnames(hu_eupqua)

for(i in 1:nrow(sites_eupqua)) {
  results_quad_eupqua <- dplyr::bind_rows(results_quad_eupqua, 
                                          as.data.frame(ins_lep_2_quad_eval(sites_eupqua[1,7], sites_eupqua[i,1])))
}

write.csv(results_quad_eupqua[c(2:nrow(results_quad_eupqua)),], 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_eupqua_quad.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

results_quad_lep_2 <- results_quad_eupqua

#|-lep_2_sci_results ----
sites_lep_2 <- sites_subjects %>%
  dplyr::filter(feature_code == 6199)
hu_lep_2 <- ins_lep_2_sci_eval(sites_lep_2[20,5],
                               sites_lep_2[20,1])
results_sci_lep_2 <- matrix(NA, 1, ncol(hu_lep_2)) %>% 
  dplyr::as_tibble()
colnames(results_sci_lep_2) <- colnames(hu_lep_2)

for(i in 1:nrow(sites_lep_2)) {
  results_sci_lep_2 <- dplyr::bind_rows(results_sci_lep_2, 
                                        as.data.frame(ins_lep_2_sci_eval(sites_lep_2[i,5], sites_lep_2[i,1])))
}
write.csv(results_sci_lep_2[c(2:nrow(results_sci_lep_2)),], 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_lep_2_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


# DATATABLE LEP ----
library(DT)
# |-Phengaris sp. ----
lep_1_quad_dt <- DT::datatable(results_quad_lep_1 %>%
                                 dplyr::group_by(SITECODE, POLE, FEATURE_CODE) %>%
                                 dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                 ROSTLINY,
                                                                                 LIKVIDACE,
                                                                                 NEGATIV,
                                                                                 MANAGEMENT)) == TRUE ~ 0,
                                                                       TRUE ~ 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                          EVAL == 0 ~ -1,
                                                                          SUFFICIENT == 1 ~ 1,
                                                                          SUFFICIENT < 1 & 
                                                                            PRESENCE > 0 ~ 0.5),
                                               PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                               PRESENCE < 0.5 & 
                                                                                 PRESENCE > 0 ~ 0.5,
                                                                               PRESENCE == 0 ~ 0,
                                                                               is.na(PRESENCE) == TRUE ~ -1),
                                               ROSTLINY_COL = dplyr::case_when(ROSTLINY >= 0.5 ~ 1,
                                                                               ROSTLINY < 0.5~ 0,
                                                                               is.na(ROSTLINY) == TRUE ~ -1),
                                               LIKVIDACE_COL = dplyr::case_when(LIKVIDACE >= 0.5 ~ 1,
                                                                                LIKVIDACE < 0.5  ~ 0,
                                                                                is.na(LIKVIDACE) == TRUE ~ -1),
                                               MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                                 MANAGEMENT < 0.5  ~ 0,
                                                                                 is.na(MANAGEMENT) == TRUE ~ -1),
                                               NEGATIV_COL = dplyr::case_when(NEGATIV >= 0.5 ~ 1,
                                                                              NEGATIV < 0.5  ~ 0,
                                                                              is.na(NEGATIV) == TRUE ~ -1)) %>%
                                 dplyr::mutate(across(c(PRESENCE,
                                                        ROSTLINY,
                                                        LIKVIDACE,
                                                        MANAGEMENT,
                                                        NEGATIV,
                                                        OVERALL,
                                                        SUFFICIENT),
                                                      round, 3)) %>%
                                 as.data.frame(),
                               extensions = 'Buttons',
                               options = list(
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel'),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Calibri'});",
                                   "}"
                                 ),
                                 columnDefs = list(list(targets = c(18:23), visible = FALSE))),
                               rownames = FALSE,
                               filter = "top") %>%
  DT::formatStyle('POLE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('FEATURE_CODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ROSTLINY', 'ROSTLINY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('LIKVIDACE', 'LIKVIDACE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NEGATIV_LIST', 'NEGATIV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NEGATIV', 'NEGATIV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MANAGEMENT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TARGET_MON',
                  backgroundColor = styleEqual(c(0, 1), 
                                               c("grey", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

lep_1_sci_dt <- DT::datatable(results_sci_lep_1 %>%
                                dplyr::group_by(SITECODE, FEATURE_CODE) %>%
                                dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                ROSTLINY,
                                                                                LIKVIDACE,
                                                                                NEGATIV,
                                                                                MANAGEMENT,
                                                                                HABITAT)) == TRUE ~ 0,
                                                                      TRUE ~ 1)) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                         EVAL == 0 ~ -1,
                                                                         SUFFICIENT == 1 ~ 1,
                                                                         SUFFICIENT < 1 & 
                                                                           PRESENCE > 0 ~ 0.5),
                                              PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                              PRESENCE < 0.5 & 
                                                                                PRESENCE > 0 ~ 0.5,
                                                                              PRESENCE == 0 ~ 0,
                                                                              is.na(PRESENCE) == TRUE ~ -1),
                                              ROSTLINY_COL = dplyr::case_when(ROSTLINY >= 0.5 ~ 1,
                                                                              ROSTLINY < 0.5~ 0,
                                                                              is.na(ROSTLINY) == TRUE ~ -1),
                                              LIKVIDACE_COL = dplyr::case_when(LIKVIDACE >= 0.5 ~ 1,
                                                                               LIKVIDACE < 0.5  ~ 0,
                                                                               is.na(LIKVIDACE) == TRUE ~ -1),
                                              MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                               MANAGEMENT < 0.5  ~ 0,
                                                                               is.na(MANAGEMENT) == TRUE ~ -1),
                                              HABITAT_COL = dplyr::case_when(HABITAT >= 0.5 ~ 1,
                                                                             HABITAT < 0.5  ~ 0,
                                                                             is.na(HABITAT) == TRUE ~ -1),
                                              NEGATIV_COL = dplyr::case_when(NEGATIV >= 0.5 ~ 1,
                                                                               NEGATIV < 0.5  ~ 0,
                                                                               is.na(NEGATIV) == TRUE ~ -1)) %>%
                                dplyr::mutate(across(c(PRESENCE,
                                                       ROSTLINY,
                                                       LIKVIDACE,
                                                       MANAGEMENT,
                                                       HABITAT_PERC,
                                                       NEGATIV,
                                                       OVERALL,
                                                       SUFFICIENT),
                                                     round, 3)) %>%
                                     as.data.frame(),
                                   extensions = 'Buttons',
                                   options = list(
                                     dom = 'Bfrtip',
                                     buttons = c('csv', 'excel'),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('body').css({'font-family': 'Calibri'});",
                                       "}"
                                     ),
                                     columnDefs = list(list(targets = c(15:21), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('FEATURE_CODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ROSTLINY', 'ROSTLINY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('LIKVIDACE', 'LIKVIDACE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NEGATIV', 'NEGATIV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MANAGEMENT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT', 'HABITAT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_PERC', 'HABITAT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))
lep_1_quad_dt
lep_1_sci_dt

# |-Euplagia quadripunctaria ----
lep_2_quad_dt <- DT::datatable(results_quad_lep_2 %>%
                                 dplyr::group_by(SITECODE, POLE, FEATURE_CODE) %>%
                                 dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                 ROSTLINY,
                                                                                 LIKVIDACE,
                                                                                 NEGATIV)) == TRUE ~ 0,
                                                                       TRUE ~ 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                          EVAL == 0 ~ -1,
                                                                          SUFFICIENT == 1 ~ 1,
                                                                          SUFFICIENT < 1 & 
                                                                            PRESENCE > 0 ~ 0.5),
                                               PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                               PRESENCE < 0.5 & 
                                                                                 PRESENCE > 0 ~ 0.5,
                                                                               PRESENCE == 0 ~ 0,
                                                                               is.na(PRESENCE) == TRUE ~ -1),
                                               ROSTLINY_COL = dplyr::case_when(ROSTLINY >= 0.5 ~ 1,
                                                                               ROSTLINY < 0.5~ 0,
                                                                               is.na(ROSTLINY) == TRUE ~ -1),
                                               LIKVIDACE_COL = dplyr::case_when(LIKVIDACE >= 0.5 ~ 1,
                                                                                LIKVIDACE < 0.5  ~ 0,
                                                                                is.na(LIKVIDACE) == TRUE ~ -1),
                                               MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                                 MANAGEMENT < 0.5  ~ 0,
                                                                                 is.na(MANAGEMENT) == TRUE ~ -1),
                                               NEGATIV_COL = dplyr::case_when(NEGATIV >= 0.5 ~ 1,
                                                                              NEGATIV < 0.5  ~ 0,
                                                                              is.na(NEGATIV) == TRUE ~ -1)) %>%
                                 dplyr::mutate(across(c(PRESENCE,
                                                        ROSTLINY,
                                                        LIKVIDACE,
                                                        MANAGEMENT,
                                                        NEGATIV,
                                                        OVERALL,
                                                        SUFFICIENT),
                                                      round, 3)) %>%
                                 as.data.frame(),
                               extensions = 'Buttons',
                               options = list(
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel'),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Calibri'});",
                                   "}"
                                 ),
                                 columnDefs = list(list(targets = c(16:21), visible = FALSE))),
                               rownames = FALSE,
                               filter = "top") %>%
  DT::formatStyle('POLE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('FEATURE_CODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ROSTLINY', 'ROSTLINY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('LIKVIDACE', 'LIKVIDACE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NEGATIV', 'NEGATIV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MANAGEMENT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TARGET_MON',
                  backgroundColor = styleEqual(c(0, 1), 
                                               c("grey", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

lep_1_sci_dt <- DT::datatable(results_sci_lep_1 %>%
                                dplyr::group_by(SITECODE, FEATURE_CODE) %>%
                                dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                ROSTLINY,
                                                                                LIKVIDACE,
                                                                                NEGATIV,
                                                                                MANAGEMENT,
                                                                                HABITAT)) == TRUE ~ 0,
                                                                      TRUE ~ 1)) %>%
                                dplyr::ungroup() %>%
                                dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                         EVAL == 0 ~ -1,
                                                                         SUFFICIENT == 1 ~ 1,
                                                                         SUFFICIENT < 1 & 
                                                                           PRESENCE > 0 ~ 0.5),
                                              PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                              PRESENCE < 0.5 & 
                                                                                PRESENCE > 0 ~ 0.5,
                                                                              PRESENCE == 0 ~ 0,
                                                                              is.na(PRESENCE) == TRUE ~ -1),
                                              ROSTLINY_COL = dplyr::case_when(ROSTLINY >= 0.5 ~ 1,
                                                                              ROSTLINY < 0.5~ 0,
                                                                              is.na(ROSTLINY) == TRUE ~ -1),
                                              LIKVIDACE_COL = dplyr::case_when(LIKVIDACE >= 0.5 ~ 1,
                                                                               LIKVIDACE < 0.5  ~ 0,
                                                                               is.na(LIKVIDACE) == TRUE ~ -1),
                                              MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                                MANAGEMENT < 0.5  ~ 0,
                                                                                is.na(MANAGEMENT) == TRUE ~ -1),
                                              HABITAT_COL = dplyr::case_when(HABITAT >= 0.5 ~ 1,
                                                                             HABITAT < 0.5  ~ 0,
                                                                             is.na(HABITAT) == TRUE ~ -1),
                                              NEGATIV_COL = dplyr::case_when(NEGATIV >= 0.5 ~ 1,
                                                                             NEGATIV < 0.5  ~ 0,
                                                                             is.na(NEGATIV) == TRUE ~ -1)) %>%
                                dplyr::mutate(across(c(PRESENCE,
                                                       ROSTLINY,
                                                       LIKVIDACE,
                                                       MANAGEMENT,
                                                       HABITAT_PERC,
                                                       NEGATIV,
                                                       OVERALL,
                                                       SUFFICIENT),
                                                     round, 3)) %>%
                                as.data.frame(),
                              extensions = 'Buttons',
                              options = list(
                                dom = 'Bfrtip',
                                buttons = c('csv', 'excel'),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$('body').css({'font-family': 'Calibri'});",
                                  "}"
                                ),
                                columnDefs = list(list(targets = c(15:21), visible = FALSE))),
                              rownames = FALSE,
                              filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('FEATURE_CODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ROSTLINY', 'ROSTLINY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('LIKVIDACE', 'LIKVIDACE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NEGATIV', 'NEGATIV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MANAGEMENT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT', 'HABITAT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_PERC', 'HABITAT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

lep_1_quad_dt
lep_1_sci_dt

# FUNKCE COL ----

# FUNKCE ORT ----

