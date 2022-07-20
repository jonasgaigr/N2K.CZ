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

phengaris_habitaty <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/phengaris_habitaty.csv", encoding = "UTF-8")

# FUNKCE ----
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
  return(nrow(subset(sites_subjects, sites_subjects$Název.česky == species)))
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.česky == species) %>%
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
find_evl_HAB_QUAL <- function(species) {
  return(phengaris_habitaty %>%
           filter(SITECODE == species) %>%
           pull(HAB_AREA)
         )
}
# SEZNAM PŘEDMĚTŮ OCHRANY EVL
find_evl_TARGETS <- function(species) {
  return(sites_subjects %>%
           filter(Typ.lokality == "EVL") %>%
           filter(Název.lokality == species) %>%
           pull(Název.česky)
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

# Motýle ----
ins_lep_1_quad_eval <- function(species_code, evl_site) {
  
  # FIND LEVEL
  species_target_find <- species %>%
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
      PLANTS_find = dplyr::case_when(grepl("<toten_pritomnost>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(ZDROJ == "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
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
    THREATS_find = dplyr::case_when(grepl("žádný vliv", STRUKT_POZN) ~ 1,
                                    TARGET_MON_find == 1 ~ 0),
    THREATS_NUM_find = stringr::str_count(STRUKT_POZN, ",")
  ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         PLANTS_find,
                         MANAGEMENT_find,
                         DESTRUCT_find,
                         THREATS_find)
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
      MANAGEMENT_site = unique(MANAGEMENT_find),
      DESTRUCT_site = unique(DESTRUCT_find),
      THREATS_site = unique(THREATS_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
           ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
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
    dplyr::group_by(POLE) %>%
    dplyr::summarise(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = species_code,
      POLE = unique(POLE),
      PRESENCE = dplyr::case_when(max(na.omit(PRESENCE_site)) == 1 ~ 1,
                                  max(na.omit(PRESENCE_site)) == 0 ~ 0),
      ROSTLINY = mean(na.omit(PLANTS_site)),
      LIKVIDACE = mean(na.omit(DESTRUCT_site)),
      NEGATIV = mean(na.omit(THREATS_site)),
      MANAGEMENT = mean(na.omit(MANAGEMENT_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site))) %>%
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

sites_phenau <- sites_subjects %>%
  filter(feature_code == 6179)

testtest <- ins_lep_1_quad_eval(sites_phenau[1,7], sites_phenau[11,1])

hu_phenau <- ins_lep_1_quad_eval(sites_phenau[1,7], sites_phenau[1,1])
results_quad_phenau <- matrix(NA, 1, ncol(hu_phenau)) %>% 
  dplyr::as_tibble()
colnames(results_quad_phenau) <- colnames(hu_phenau)

for(i in 1:nrow(sites_phenau)) {
  results_quad_phenau <- dplyr::bind_rows(results_quad_phenau, 
                                          as.data.frame(ins_lep_1_quad_eval("Phengaris nausithous", sites_phenau[i,1])))
}
write.csv(results_quad_phenau[c(2:nrow(results_quad_phenau)),], 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_phenau_quad.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

# Phengaris teleius
sites_phetel <- sites_subjects %>%
  filter(feature_code == 6177)
hu_phetel <- ins_lep_1_quad_eval(sites_phetel[1,7], sites_phetel[1,1])
results_quad_phetel <- matrix(NA, 1, ncol(hu_phetel)) %>% 
  dplyr::as_tibble()
colnames(results_quad_phetel) <- colnames(hu)

for(i in 1:nrow(sites_phetel)) {
  results_quad_phetel <- dplyr::bind_rows(results_quad_phetel, 
                                          as.data.frame(ins_lep_1_quad_eval(sites_phetel[1,7], sites_phetel[i,1])))
}

ins_lep_1_sci_eval <- function(species_code, evl_site) {
  
  # FIND LEVEL
  species_target_find <- species %>%
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
      PLANTS_find = dplyr::case_when(grepl("<toten_pritomnost>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(ZDROJ == "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
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
      THREATS_find = dplyr::case_when(grepl("žádný vliv", STRUKT_POZN) ~ 1,
                                      TARGET_MON_find == 1 ~ 0),
      THREATS_NUM_find = stringr::str_count(STRUKT_POZN, ",")
    ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         PLANTS_find,
                         MANAGEMENT_find,
                         DESTRUCT_find,
                         THREATS_find)
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
      MANAGEMENT_site = unique(MANAGEMENT_find),
      DESTRUCT_site = unique(DESTRUCT_find),
      THREATS_site = unique(THREATS_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_quad <- species_target_find %>%
    dplyr::group_by(POLE) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
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
    dplyr::group_by(POLE) %>%
    dplyr::summarise(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = species_code,
      POLE = unique(POLE),
      PRESENCE = dplyr::case_when(max(na.omit(PRESENCE_site)) == 1 ~ 1,
                                  max(na.omit(PRESENCE_site)) == 0 ~ 0),
      ROSTLINY = mean(na.omit(PLANTS_site)),
      LIKVIDACE = mean(na.omit(DESTRUCT_site)),
      NEGATIV = mean(na.omit(THREATS_site)),
      MANAGEMENT = mean(na.omit(MANAGEMENT_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site))) %>%
    dplyr::ungroup()
  
  species_target_sci <- species_target_quad %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::summarise(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = species_code,
      PRESENCE = mean(na.omit(PRESENCE)),
      ROSTLINY = mean(na.omit(ROSTLINY)),
      LIKVIDACE = mean(na.omit(LIKVIDACE)),
      NEGATIV = mean(na.omit(NEGATIV)),
      MANAGEMENT = mean(na.omit(MANAGEMENT)),
      TARGET_MON = max(last_targeted_mon_quad$LAST_TARGET_MON),
      OVERALL = mean(na.omit(OVERALL))) %>%
    dplyr::ungroup()
  
  result <- species_target_sci
  
  result
  
}
testtest <- ins_lep_1_sci_eval(sites_phenau[1,7], sites_phenau[11,1])
