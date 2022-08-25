setwd("C:/Users/jonas.gaigr/N2K.CZ")
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

# FUNKCE BRY ----
bry_buxvir_site_eval <- function(sci_code) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(SITECODE == sci_code) %>%
    filter(DRUH == "Buxbaumia viridis") %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      ABUNDANCE_find = dplyr::case_when(POCET >= 3 ~ 1,
                                        POCET < 3 ~ 0), 
      DEADWOOD_find = dplyr::case_when(grepl("<SUB>nedostačující</SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                       grepl("<SUB>dostačující</SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(grepl("<SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                         TRUE ~ 0),
    ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         ABUNDANCE_find,
                         DEADWOOD_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_site <- species_target_find %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::summarise(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      DRUH = "Buxbaumia viridis",
      PRESENCE_site = unique(PRESENCE_find),
      ABUNDANCE_site = unique(ABUNDANCE_find),
      DEADWOOD_site = unique(DEADWOOD_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_site <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_TARGET_MON)
  
  last_find_site <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_FIND)
  
  
  result <- species_target_site %>%
    dplyr::mutate(SUFFICIENT_site = dplyr::case_when(OVERALL_site == 3 ~ 1,
                                                     OVERALL_site < 3 ~ 0)) %>%
    dplyr::full_join(.,
                     last_targeted_mon_site, 
                     by = "LOKALITA") %>%
    dplyr::full_join(., 
                     last_find_site, 
                     by = "LOKALITA") %>%
    dplyr::distinct()
  
  result
  
}

bry_buxvir_sci_eval <- function(sci_code) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(SITECODE == sci_code) %>%
    filter(DRUH == "Buxbaumia viridis") %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      ABUNDANCE_find = dplyr::case_when(POCET >= 3 ~ 1,
                                        POCET < 3 ~ 0),
      DEADWOOD_find = dplyr::case_when(grepl("<SUB>nedostačující</SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                       grepl("<SUB>dostačující</SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(grepl("<SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                        TRUE ~ 0),
    ) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         ABUNDANCE_find,
                         DEADWOOD_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_sites <- species_target_find %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::filter(YEAR >= 2021 - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(YEAR)) %>%
    dplyr::arrange(desc(PRESENCE_find)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(
      PRESENCE_site = unique(PRESENCE_find),
      ABUNDANCE_site = unique(ABUNDANCE_find),
      DEADWOOD_site = unique(DEADWOOD_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_sci <- species_target_find %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::summarise(SITECODE = sci_code,
                     LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, LAST_TARGET_MON)
  
  last_find_sci <- species_target_find %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(SITECODE = sci_code,
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, LAST_FIND)
  
  sufficient_sites <- species_target_sites %>%
    dplyr::filter(OVERALL_site == 3) %>%
    pull(LOKALITA) %>%
    unique()
  
  sufficient_sites_length <- sufficient_sites %>%
    length()
  
  species_target_sci <- species_target_sites %>%
    dplyr::summarise(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      DRUH = "Buxbaumia viridis",
      PRESENCE = mean(na.omit(PRESENCE_site)),
      ABUNDANCE = mean(na.omit(ABUNDANCE_site)),
      DEADWOOD = mean(na.omit(DEADWOOD_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site)),
      SUFFICIENT = sufficient_sites_length) %>%
    dplyr::ungroup()
  
  result <- species_target_sci %>%
    dplyr::full_join(.,
                     last_targeted_mon_sci, 
                     by = "SITECODE") %>%
    dplyr::full_join(., 
                     last_find_sci, 
                     by = "SITECODE") %>%
    dplyr::distinct()
  
  result
  
}

bry_dicvir_site_eval <- function(sci_code) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(SITECODE == sci_code) %>%
    filter(DRUH == "Dicranum viride") %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      MICROSITE_num = readr::parse_number(gsub(".*<pocet_ml>|</pocet_ml>.*", "", STRUKT_POZN)),
      ABUNDANE_num = POCET,
      ABUNDANCE_uni = POCITANO,
      ABUNDANCE_find = dplyr::case_when(POCET >= 10 & POCITANO == "cm2" ~ 1,
                                        POCET < 10 & POCITANO == "cm2" ~ 0),
      TREND_value = readr::parse_number(gsub(".*<trend_vyvoj>>|</trend_vyvoj>.*", "", STRUKT_POZN)),
      TREND_find = dplyr::case_when(grepl("<trend_vyvoj>zvyšujícící</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                    grepl("<trend_vyvoj>kolísající</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                    grepl("<trend_vyvoj>snižující</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 0),
      STRUCTAGE_find = dplyr::case_when(grepl("<str_strom>nevyhovující</str_strom>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                        grepl("<str_strom>vyhovující</str_strom>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      STRUCTSPE_find = dplyr::case_when(grepl("<druh_strom>nevyhovující</druh_strom>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                        grepl("<druh_strom>vyhovující</druh_strom>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      MANAGEMENT_find = dplyr::case_when(grepl("<MAN>bezzásahovost</MAN>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                         grepl("<MAN>výběrová těžba</MAN>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      THREATS_find = dplyr::case_when(grepl("<NEG>nepozorovány</NEG>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      TARGET_MON_find = dplyr::case_when(PROJEKT == "Monitoring druhů ČR" &
                                           grepl("<SUB>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                         TRUE ~ 0),
    ) %>%
    dplyr::mutate(MICROSITE_find = dplyr::case_when(MICROSITE_num > 10 ~ 1,
                                                    MICROSITE_num <= 10 ~ 0)) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(PRESENCE_find, 
                         MICROSITE_find,
                         ABUNDANCE_find,
                         TREND_find,
                         STRUCTAGE_find,
                         STRUCTSPE_find,
                         MANAGEMENT_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_site <- species_target_find %>%
    dplyr::filter(YEAR >= 2021 - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(TARGET_MON_find),
                   desc(YEAR)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(
      SITECODE = unique(SITECODE),
      PRESENCE_site = unique(PRESENCE_find), 
      MICROSITE_site = unique(MICROSITE_find),
      ABUNDANCE_site = unique(ABUNDANCE_find),
      TREND_site = unique(TREND_find),
      STRUCTAGE_site = unique(STRUCTAGE_find),
      STRUCTSPE_site = unique(STRUCTSPE_find),
      MANAGEMENT_site = unique(MANAGEMENT_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_site <- species_target_find %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_TARGET_MON)
  
  last_find_site <- species_target_find %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_FIND)

  species_target_site_ID <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(TARGET_MON_find),
                   desc(YEAR)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     ID_ND_NALEZ = unique(ID_ND_NALEZ)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, ID_ND_NALEZ)
  
  result <- species_target_site %>%
    dplyr::full_join(.,
                     species_target_site_ID, 
                     by = "LOKALITA") %>%
    dplyr::full_join(.,
                     last_targeted_mon_site, 
                     by = "LOKALITA") %>%
    dplyr::full_join(., 
                     last_find_site, 
                     by = "LOKALITA")
  
  result
  
}

bry_dicvir_sci_eval <- function(sci_code) {
 
  # SITE LEVEL
  species_target_sites <- results_sites_dicvir %>%
    dplyr::filter(SITECODE == sci_code)
  
  sufficient_sites <- species_target_sites %>%
    dplyr::filter(OVERALL_site == 7) %>%
    dplyr::pull(LOKALITA) %>%
    unique()
  
  sufficient_sites_length <- sufficient_sites %>%
    length()
  
  total_sites <- species_target_sites %>%
    dplyr::filter(TARGET_MON_site == 1) %>%
    dplyr::mutate(YEAR = substring(LAST_TARGET_MON, 1, 4)) %>%
    dplyr::filter(YEAR > 2021 - 6) %>%
    dplyr::pull(LOKALITA) %>%
    unique()
  
  total_sites_length <- total_sites %>%
    length()
  
  id_nd_list <- species_target_sites %>%
    dplyr::filter(TARGET_MON_site == 1) %>%
    dplyr::mutate(YEAR = substring(LAST_TARGET_MON, 1, 4)) %>%
    dplyr::filter(YEAR > 2021 - 6) %>%
    pull(ID_ND_NALEZ) %>%
    unique() %>%
    toString()
  
  species_target_sci <- species_target_sites %>%
    dplyr::summarise(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      DRUH = "Dicranum viride",
      PRESENCE = mean(na.omit(PRESENCE_site)),
      MICROSITE = mean(na.omit(MICROSITE_site)),
      ABUNDANCE = mean(na.omit(ABUNDANCE_site)),
      TREND = mean(na.omit(TREND_site)),
      STRUKT_VEK = mean(na.omit(STRUCTAGE_site)),
      STRUKT_DRU = mean(na.omit(STRUCTSPE_site)),
      MANAGEMENT = mean(na.omit(MANAGEMENT_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site)),
      SUFFICIENT = sufficient_sites_length,
      TOTAL_SITES = total_sites_length,
      SUFFICIENT_PERC = sufficient_sites_length/total_sites_length,
      ID_ND_NALEZ = id_nd_list,
      LAST_TARGET_MON = max(na.omit(LAST_TARGET_MON)),
      LAST_FIND = max(LAST_FIND)
      ) %>%
    dplyr::ungroup()
  
  result <- species_target_sci
  
  result
  
}

bry_hamver_site_eval <- function(sci_code) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    filter(SITECODE == sci_code) %>%
    filter(DRUH == "Hamatocaulis vernicosus") %>%
    mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      MICROSITE_num = readr::parse_number(gsub(".*<plocha_populace>|</plocha_populace>.*", "", STRUKT_POZN)),
      MICROSITE_uni = readr::parse_character(gsub(".*<plocha_populace>|</plocha_populace>.*", "", STRUKT_POZN)),
      ABUNDANCE_num = POCET,
      ABUNDANCE_uni = POCITANO,
      ABUNDANCE_find = dplyr::case_when(POCET >= 500 & POCITANO == "cm2" ~ 1,
                                        POCET >= 5 & POCITANO == "dm2" ~ 1,
                                        POCET >= 0.05 & POCITANO == "m2" ~ 1,
                                        POCET < 500 & POCITANO == "cm2" ~ 0,
                                        POCET < 5 & POCITANO == "dm2" ~ 0,
                                        POCET < 0.05 & POCITANO == "m2" ~ 0),
      TREND_find = dplyr::case_when(grepl("<trend_vyvoj>zvyšujícící</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                    grepl("<trend_vyvoj>kolísající</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                    grepl("<trend_vyvoj>snižující</trend_vyvoj>", STRUKT_POZN, ignore.case = TRUE) ~ 0),
      CHANGE1_num = readr::parse_number(gsub(".*<vel_pop>|</vel_pop>.*", "", STRUKT_POZN)),
      CHANGE2_num = readr::parse_number(gsub(".*<vel_pop2>|</vel_pop2>.*", "", STRUKT_POZN)),
      STRUCTURE_find = dplyr::case_when(grepl("<str_strom>nevyhovující</str_strom>", STRUKT_POZN, ignore.case = TRUE) |
                                          grepl("<druh_strom>nevyhovující</druh_strom>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                        grepl("<str_strom>vyhovující</str_strom>", STRUKT_POZN, ignore.case = TRUE) &
                                          grepl("<druh_strom>vyhovující</druh_strom>", STRUKT_POZN, ignore.case = TRUE)~ 1),
      POTENTIAL_num = readr::parse_number(gsub(".*<velikost_pl>|</velikost_pl>.*", "", STRUKT_POZN)),
      PLANTCOV_num = readr::parse_number(gsub(".*<pokr_bylin>|</pokr_bylin>.*", "", STRUKT_POZN)),
      EXPCOV_num = readr::parse_number(gsub(".*<pokr_exp_dr>|</pokr_exp_dr>.*", "", STRUKT_POZN)),
      SHADECOV_num = readr::parse_number(gsub(".*<zast_sta>|</zast_sta>.*", "", STRUKT_POZN)),
      MANAGEMENT_find = dplyr::case_when(grepl("<MAN>nedostačující</MAN>", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                         grepl("<MAN>dostačující</MAN>", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      THREATS_find = dplyr::case_when(grepl("<NEG>nepozorovány</NEG>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                      grepl("zarůstání", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                      TRUE ~ 1),
      TARGET_MON_find = dplyr::case_when(PROJEKT == "Monitoring druhů ČR" & 
                                           grepl("<MAN>", STRUKT_POZN, ignore.case = TRUE) &
                                           grepl("<pokr_exp_dr>", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                         TRUE ~ 0),
    ) %>%
    dplyr::mutate(MICROSITE_find = dplyr::case_when(MICROSITE_num >= 10 ~ 1,
                                                    MICROSITE_num < 10 ~ 0),
                  CHANGE1_find = dplyr::case_when(CHANGE1_num > -10 ~ 1,
                                                  CHANGE1_num <= -10 ~ 0),
                  CHANGE2_find = dplyr::case_when(CHANGE2_num > -30 ~ 1,
                                                  CHANGE2_num <= -30 ~ 0),
                  POTENTIAL_find = dplyr::case_when(POTENTIAL_num >= 500 ~ 1,
                                                    POTENTIAL_num < 500 ~ 0),
                  PLANTCOV_find = dplyr::case_when(PLANTCOV_num <= 90 ~ 1,
                                                   PLANTCOV_num > 90 ~ 0),
                  EXPCOV_find = dplyr::case_when(EXPCOV_num <= 10 ~ 1,
                                                 EXPCOV_num > 10 ~ 0),
                  SHADECOV_find = dplyr::case_when(SHADECOV_num < 5 ~ 1,
                                                   SHADECOV_num >= 5 ~ 0)) %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      OVERALL_find = sum(ABUNDANCE_find,
                         MICROSITE_find,
                         CHANGE1_find,
                         CHANGE2_find,
                         POTENTIAL_find,
                         PLANTCOV_find,
                         EXPCOV_find,
                         SHADECOV_find,
                         MANAGEMENT_find,
                         na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_site <- species_target_find %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::filter(YEAR >= 2021 - 3) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(DATE)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(
      SITECODE = unique(SITECODE),
      PRESENCE_site = unique(PRESENCE_find), 
      ABUNDANCE_val = paste(unique(ABUNDANCE_num), unique(ABUNDANCE_uni), sep = " "),
      ABUNDANCE_site = unique(ABUNDANCE_find),
      MICROSITE_val = paste(unique(MICROSITE_num), "m2", sep = " "),
      MICROSITE_site = unique(MICROSITE_find),
      CHANGE1_site = unique(CHANGE1_find),
      CHANGE2_site = unique(CHANGE2_find),
      POTENTIAL_site = unique(POTENTIAL_find),
      PLANTCOV_val = paste(unique(PLANTCOV_num), "%", sep = " "),
      PLANTCOV_site = unique(PLANTCOV_find),
      EXPCOV_val = paste(unique(EXPCOV_num), "%", sep = " "),
      EXPCOV_site = unique(EXPCOV_find),
      SHADECOV_val = paste(unique(SHADECOV_num), "%", sep = " "),
      SHADECOV_site = unique(SHADECOV_find),
      MANAGEMENT_site = unique(MANAGEMENT_find),
      OVERALL_site = unique(OVERALL_find),
      TARGET_MON_site = unique(TARGET_MON_find)
    ) %>%
    dplyr::ungroup()
  
  last_targeted_mon_site <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::filter(TARGET_MON_find == 1) %>%
    dplyr::summarise(LAST_TARGET_MON = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_TARGET_MON)
  
  last_find_site <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, LAST_FIND)
  
  species_target_site_ID <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(TARGET_MON_find),
                   desc(YEAR)) %>%
    dplyr::slice(1) %>%
    dplyr::summarise(LOKALITA = unique(LOKALITA),
                     ID_ND_NALEZ = unique(ID_ND_NALEZ)) %>%
    dplyr::ungroup() %>%
    dplyr::select(LOKALITA, ID_ND_NALEZ)
  
  result <- species_target_site %>%
    dplyr::full_join(.,
                     species_target_site_ID, 
                     by = "LOKALITA") %>%
    dplyr::full_join(.,
                     last_targeted_mon_site, 
                     by = "LOKALITA") %>%
    dplyr::full_join(., 
                     last_find_site, 
                     by = "LOKALITA")
  
  result
  
}

bry_hamver_sci_eval <- function(sci_code) {

  species_target_sites <- results_sites_hamver %>%
    dplyr::filter(SITECODE == sci_code)
  
  sufficient_sites <- species_target_sites %>%
    dplyr::filter(OVERALL_site == 9) %>%
    dplyr::pull(LOKALITA) %>%
    unique()
  
  sufficient_sites_length <- sufficient_sites %>%
    length()
  
  total_sites <- species_target_sites %>%
    dplyr::filter(TARGET_MON_site == 1) %>%
    dplyr::mutate(YEAR = substring(LAST_TARGET_MON, 1, 4)) %>%
    dplyr::filter(YEAR > 2021 - 3) %>%
    dplyr::pull(LOKALITA) %>%
    unique()
  
  total_sites_length <- total_sites %>%
    length()
  
  id_nd_list <- species_target_sites %>%
    dplyr::filter(TARGET_MON_site == 1) %>%
    dplyr::mutate(YEAR = substring(LAST_TARGET_MON, 1, 4)) %>%
    dplyr::filter(YEAR > 2021 - 3) %>%
    pull(ID_ND_NALEZ) %>%
    unique() %>%
    toString()
  
  species_target_sci <- species_target_sites %>%
    dplyr::summarise(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      DRUH = "Hamatocaulis vernicosus",
      PRESENCE = mean(na.omit(PRESENCE_site)),
      ABUNDANCE = mean(na.omit(ABUNDANCE_site)),
      MICROSITE = mean(na.omit(MICROSITE_site)),
      CHANGE1 = mean(na.omit(CHANGE1_site)),
      CHANGE2 = mean(na.omit(CHANGE2_site)),
      POTENTIAL = mean(na.omit(POTENTIAL_site)),
      PLANTCOV = mean(na.omit(PLANTCOV_site)),
      EXPCOV = mean(na.omit(EXPCOV_site)),
      SHADECOV = mean(na.omit(SHADECOV_site)),
      MANAGEMENT = mean(na.omit(MANAGEMENT_site)),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      OVERALL = mean(na.omit(OVERALL_site)),
      SUFFICIENT = sufficient_sites_length,
      TOTAL_SITES = total_sites_length,
      SUFFICIENT_PERC = sufficient_sites_length/total_sites_length,
      ID_ND_NALEZ = id_nd_list,
      LAST_TARGET_MON = max(na.omit(LAST_TARGET_MON)),
      LAST_FIND = max(LAST_FIND)) %>%
    dplyr::ungroup()
  
  result <- species_target_sci
  
  result
  
}



# RESULTS ----
# |-Buxbaumia viridis SITES ----
sites_buxvir <- sites_subjects %>%
  filter(feature_code == 1386)
hu_buxvir <- bry_buxvir_site_eval(sites_buxvir[1,1])
results_sites_buxvir <- matrix(NA, 1, ncol(hu_buxvir)) %>% 
  dplyr::as_tibble()
colnames(results_sites_buxvir) <- colnames(hu_buxvir)

for(i in 1:nrow(sites_buxvir)) {
  results_sites_buxvir <- dplyr::bind_rows(results_sites_buxvir, 
                                         as.data.frame(bry_buxvir_site_eval(sites_buxvir[i,1])))
}
results_sites_buxvir <- results_sites_buxvir[c(2:nrow(results_sites_buxvir)),]
write.csv(results_sites_buxvir, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_buxvir_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sci_buxvir[c(2:nrow(results_sci_buxvir)),], 
           "C:/Users/jonas.gaigr/Desktop/results_sci_buxvir.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# |-Buxbaumia viridis SCI ----
sites_buxvir <- sites_subjects %>%
  filter(feature_code == 1386)
hu_buxvir <- bry_buxvir_sci_eval(sites_buxvir[1,1])
results_sci_buxvir <- matrix(NA, 1, ncol(hu_buxvir)) %>% 
  dplyr::as_tibble()
colnames(results_sci_buxvir) <- colnames(hu_buxvir)

for(i in 1:nrow(sites_buxvir)) {
  results_sci_buxvir <- dplyr::bind_rows(results_sci_buxvir, 
                                         as.data.frame(bry_buxvir_sci_eval(sites_buxvir[i,1])))
}

write.csv(results_sci_buxvir[c(2:nrow(results_sci_buxvir)),], 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_buxvir_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sci_buxvir[c(2:nrow(results_sci_buxvir)),], 
           "C:/Users/jonas.gaigr/Desktop/results_sci_buxvir.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# |-Dicranum viridae SITES ----
sites_dicvir <- sites_subjects %>%
  filter(feature_code == 1381)
hu_dicvir <- bry_dicvir_site_eval(sites_dicvir[1,1])
results_sites_dicvir <- matrix(NA, 1, ncol(hu_dicvir)) %>% 
  dplyr::as_tibble()
colnames(results_sites_dicvir) <- colnames(hu_dicvir)

for(i in 1:nrow(sites_dicvir)) {
  results_sites_dicvir <- dplyr::bind_rows(results_sites_dicvir, 
                                         as.data.frame(bry_dicvir_site_eval(sites_dicvir[i,1])))
}
results_sites_dicvir <- results_sites_dicvir[c(2:nrow(results_sites_dicvir)),]
write.csv(results_sites_dicvir, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_dicvir_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sites_dicvir, 
           "C:/Users/jonas.gaigr/Desktop/results_sites_dicvir.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# |-Dicranum viridae SCI ----
sites_dicvir <- sites_subjects %>%
  filter(feature_code == 1381)
hu_dicvir <- bry_dicvir_sci_eval(sites_dicvir[1,1])
results_sci_dicvir <- matrix(NA, 1, ncol(hu_dicvir)) %>% 
  dplyr::as_tibble()
colnames(results_sci_dicvir) <- colnames(hu_dicvir)

for(i in 1:nrow(sites_dicvir)) {
  results_sci_dicvir <- dplyr::bind_rows(results_sci_dicvir, 
                                         as.data.frame(bry_dicvir_sci_eval(sites_dicvir[i,1])))
}
results_sci_dicvir <- results_sci_dicvir[c(2:nrow(results_sci_dicvir)),]
write.csv(results_sci_dicvir, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_dicvir_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sci_dicvir, 
           "C:/Users/jonas.gaigr/Desktop/results_sci_dicvir.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# |-Hamatocaulis vernicosus SCI ----
sites_hamver <- sites_subjects %>%
  filter(feature_code == 6216)
hu_hamver <- bry_hamver_site_eval(sites_hamver[1,1])
results_sites_hamver <- matrix(NA, 1, ncol(hu_hamver)) %>% 
  dplyr::as_tibble()
colnames(results_sites_hamver) <- colnames(hu_hamver)

for(i in 1:nrow(sites_hamver)) {
  results_sites_hamver <- dplyr::bind_rows(results_sites_hamver, 
                                         as.data.frame(bry_hamver_site_eval(sites_hamver[i,1])))
}
results_sites_hamver <- results_sites_hamver[c(2:nrow(results_sites_hamver)),]

write.csv(results_sites_hamver, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_hamver_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sites_hamver, 
           "C:/Users/jonas.gaigr/Desktop/results_sites_hamver.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# |-Hamatocaulis vernicosus SCI ----
sites_hamver <- sites_subjects %>%
  filter(feature_code == 6216)
hu_hamver <- bry_hamver_sci_eval(sites_hamver[1,1])
results_sci_hamver <- matrix(NA, 1, ncol(hu_hamver)) %>% 
  dplyr::as_tibble()
colnames(results_sci_hamver) <- colnames(hu_hamver)

for(i in 1:nrow(sites_hamver)) {
  results_sci_hamver <- dplyr::bind_rows(results_sci_hamver, 
                                         as.data.frame(bry_hamver_sci_eval(sites_hamver[i,1])))
}
results_sci_hamver <- results_sci_hamver[c(2:nrow(results_sci_hamver)),]

write.csv(results_sci_hamver, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_hamver_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sci_hamver, 
           "C:/Users/jonas.gaigr/Desktop/results_sci_hamver.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# DATATABLE ----
library(DT)
# |-Buxbaumia viridis ----
bry_buxvir_sci_dt <- DT::datatable(results_sci_buxvir %>%
                                 dplyr::group_by(SITECODE) %>%
                                 dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                 ABUNDANCE,
                                                                                 DEADWOOD)) == TRUE ~ 0,
                                                                       TRUE ~ 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                          EVAL == 0 ~ -1,
                                                                          SUFFICIENT >= 2 ~ 1,
                                                                          SUFFICIENT < 2 & 
                                                                            PRESENCE > 0 ~ 0.5),
                                               PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                               PRESENCE < 0.5 & 
                                                                                 PRESENCE > 0 ~ 0.5,
                                                                               PRESENCE == 0 ~ 0,
                                                                               is.na(PRESENCE) == TRUE ~ -1),
                                               ABUNDANCE_COL = dplyr::case_when(ABUNDANCE >= 0.5 ~ 1,
                                                                                ABUNDANCE < 0.5~ 0,
                                                                               is.na(ABUNDANCE) == TRUE ~ -1),
                                               DEADWOOD_COL = dplyr::case_when(DEADWOOD >= 0.5 ~ 1,
                                                                                DEADWOOD < 0.5  ~ 0,
                                                                               is.na(DEADWOOD) == TRUE ~ -1)) %>%
                                 dplyr::mutate(across(c(PRESENCE,
                                                        ABUNDANCE,
                                                        DEADWOOD,
                                                        OVERALL),
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
                                 columnDefs = list(list(targets = c(12:15), visible = FALSE))),
                               rownames = FALSE,
                               filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
 DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'ABUNDANCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DEADWOOD', 'DEADWOOD_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
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

bry_buxvir_sci_dt

# |-Dicranum viridae ----
bry_dicvir_sci_dt <- DT::datatable(results_sci_dicvir %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     MICROSITE,
                                                                                     ABUNDANCE,
                                                                                     TREND,
                                                                                     STRUCTURE,
                                                                                     MANAGEMENT,
                                                                                     THREATS)) == TRUE ~ 0,
                                                                           TRUE ~ 1)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.75 ~ 1,
                                                                              SUFFICIENT_PERC < 0.75 & 
                                                                                PRESENCE > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                                   PRESENCE < 0.5 & 
                                                                                     PRESENCE > 0 ~ 0.5,
                                                                                   PRESENCE == 0 ~ 0,
                                                                                   is.na(PRESENCE) == TRUE ~ -1),
                                                   MICROSITE_COL = dplyr::case_when(MICROSITE >= 0.5 ~ 1,
                                                                                    MICROSITE < 0.5~ 0,
                                                                                    is.na(MICROSITE) == TRUE ~ -1),
                                                   ABUNDANCE_COL = dplyr::case_when(ABUNDANCE >= 0.5 ~ 1,
                                                                                    ABUNDANCE < 0.5  ~ 0,
                                                                                    is.na(ABUNDANCE) == TRUE ~ -1),
                                                   TREND_COL = dplyr::case_when(TREND >= 0.5 ~ 1,
                                                                                TREND < 0.5  ~ 0,
                                                                                is.na(TREND) == TRUE ~ -1),
                                                   STRUCTURE_COL = dplyr::case_when(STRUCTURE >= 0.5 ~ 1,
                                                                                    STRUCTURE < 0.5~ 0,
                                                                                    is.na(STRUCTURE) == TRUE ~ -1),
                                                   MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                                     MANAGEMENT < 0.5~ 0,
                                                                                     is.na(MANAGEMENT) == TRUE ~ -1),
                                                   THREATS_COL = dplyr::case_when(THREATS >= 0.5 ~ 1,
                                                                                  THREATS < 0.5~ 0,
                                                                                  is.na(THREATS) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            MICROSITE,
                                                            ABUNDANCE,
                                                            TREND,
                                                            STRUCTURE,
                                                            MANAGEMENT,
                                                            THREATS,
                                                            OVERALL),
                                                          round, 3)) %>%
                                     as.data.frame(),
                                   extensions = 'Buttons',
                                   caption = "Dicranum viride",
                                   options = list(
                                     dom = 'Bfrtip',
                                     buttons = c('csv', 'excel'),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('body').css({'font-family': 'Calibri'});",
                                       "}"
                                     ),
                                     columnDefs = list(list(targets = c(18:25), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MICROSITE', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'ABUNDANCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TREND', 'TREND_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('STRUCTURE', 'STRUCTURE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MANAGEMENT_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('THREATS', 'THREATS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TARGET_MON',
                  backgroundColor = styleEqual(c(0, 1), 
                                               c("grey", "green"))) %>%
  DT::formatStyle('SUFFICIENT', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

bry_dicvir_sci_dt

# |-Hamatocaulis vernicosus ----
bry_hamver_sites_dt <- DT::datatable(results_sites_hamver %>%
                                     dplyr::group_by(LOKALITA) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_site,
                                                                                     ABUNDANCE_site,
                                                                                     MICROSITE_site,
                                                                                     CHANGE1_site,
                                                                                     CHANGE2_site,
                                                                                     POTENTIAL_site,
                                                                                     PLANTCOV_site,
                                                                                     EXPCOV_site,
                                                                                     SHADECOV_site,
                                                                                     MANAGEMENT_site)) == TRUE ~ 0,
                                                                           TRUE ~ 1)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE_site == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              OVERALL_site >= 9 ~ 1,
                                                                              OVERALL_site < 9 & 
                                                                                PRESENCE_site > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_site >= 0.5 ~ 1,
                                                                                   PRESENCE_site < 0.5 & 
                                                                                     PRESENCE_site > 0 ~ 0.5,
                                                                                   PRESENCE_site == 0 ~ 0,
                                                                                   is.na(PRESENCE_site) == TRUE ~ -1),
                                                   ABUNDANCE_COL = dplyr::case_when(ABUNDANCE_site >= 0.5 ~ 1,
                                                                                    ABUNDANCE_site < 0.5~ 0,
                                                                                    is.na(ABUNDANCE_site) == TRUE ~ -1),
                                                   MICROSITE_COL = dplyr::case_when(MICROSITE_site >= 0.5 ~ 1,
                                                                                    MICROSITE_site < 0.5~ 0,
                                                                                    is.na(MICROSITE_site) == TRUE ~ -1),
                                                   CHANGE1_COL = dplyr::case_when(CHANGE1_site >= 0.5 ~ 1,
                                                                                  CHANGE1_site < 0.5~ 0,
                                                                                  is.na(CHANGE1_site) == TRUE ~ -1),
                                                   CHANGE2_COL = dplyr::case_when(CHANGE2_site >= 0.5 ~ 1,
                                                                                  CHANGE2_site < 0.5~ 0,
                                                                                  is.na(CHANGE2_site) == TRUE ~ -1),
                                                   POTENTIAL_COL = dplyr::case_when(POTENTIAL_site >= 0.5 ~ 1,
                                                                                    POTENTIAL_site < 0.5~ 0,
                                                                                    is.na(POTENTIAL_site) == TRUE ~ -1),
                                                   PLANTCOV_COL = dplyr::case_when(PLANTCOV_site >= 0.5 ~ 1,
                                                                                   PLANTCOV_site < 0.5~ 0,
                                                                                   is.na(PLANTCOV_site) == TRUE ~ -1),
                                                   EXPCOV_COL = dplyr::case_when(EXPCOV_site >= 0.5 ~ 1,
                                                                                 EXPCOV_site < 0.5~ 0,
                                                                                 is.na(EXPCOV_site) == TRUE ~ -1),
                                                   SHADECOV_COL = dplyr::case_when(SHADECOV_site >= 0.5 ~ 1,
                                                                                   SHADECOV_site < 0.5 ~ 0,
                                                                                   is.na(SHADECOV_site) == TRUE ~ -1),
                                                   MANAGEMENT_COL = dplyr::case_when(MANAGEMENT_site >= 0.5 ~ 1,
                                                                                     MANAGEMENT_site < 0.5~ 0,
                                                                                     is.na(MANAGEMENT_site) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_site,
                                                            ABUNDANCE_site,
                                                            MICROSITE_site,
                                                            CHANGE1_site,
                                                            CHANGE2_site,
                                                            POTENTIAL_site,
                                                            PLANTCOV_site,
                                                            EXPCOV_site,
                                                            SHADECOV_site,
                                                            MANAGEMENT_site,
                                                            OVERALL_site),
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
                                     columnDefs = list(list(targets = c(23:33), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_site', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE_val', 'ABUNDANCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE_site', 'ABUNDANCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MICROSITE_val', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MICROSITE_site', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE1_site', 'CHANGE1_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE2_site', 'CHANGE2_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POTENTIAL_site', 'POTENTIAL_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PLANTCOV_val', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PLANTCOV_site', 'PLANTCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('EXPCOV_val', 'EXPCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('EXPCOV_site', 'EXPCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SHADECOV_val', 'SHADECOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SHADECOV_site', 'EXPCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT_site', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL_site', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TARGET_MON_site',
                  backgroundColor = styleEqual(c(0, 1), 
                                               c("grey", "green"))) %>%
  DT::formatStyle('OVERALL_site', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))
bry_hamver_sites_dt

bry_hamver_sci_dt <- DT::datatable(results_sci_hamver %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     ABUNDANCE,
                                                                                     MICROSITE,
                                                                                     CHANGE1,
                                                                                     CHANGE2,
                                                                                     POTENTIAL,
                                                                                     PLANTCOV,
                                                                                     EXPCOV,
                                                                                     SHADECOV,
                                                                                     MANAGEMENT)) == TRUE ~ 0,
                                                                           TRUE ~ 1)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate(ROW_COL = dplyr::case_when(PRESENCE == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT >= 1 ~ 1,
                                                                              SUFFICIENT < 1 & 
                                                                                PRESENCE > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE >= 0.5 ~ 1,
                                                                                   PRESENCE < 0.5 & 
                                                                                     PRESENCE > 0 ~ 0.5,
                                                                                   PRESENCE == 0 ~ 0,
                                                                                   is.na(PRESENCE) == TRUE ~ -1),
                                                   ABUNDANCE_COL = dplyr::case_when(ABUNDANCE >= 0.5 ~ 1,
                                                                                    ABUNDANCE < 0.5~ 0,
                                                                                    is.na(ABUNDANCE) == TRUE ~ -1),
                                                   MICROSITE_COL = dplyr::case_when(MICROSITE >= 0.5 ~ 1,
                                                                                    MICROSITE < 0.5~ 0,
                                                                                    is.na(MICROSITE) == TRUE ~ -1),
                                                   CHANGE1_COL = dplyr::case_when(CHANGE1 >= 0.5 ~ 1,
                                                                                  CHANGE1 < 0.5~ 0,
                                                                                  is.na(CHANGE1) == TRUE ~ -1),
                                                   CHANGE2_COL = dplyr::case_when(CHANGE2 >= 0.5 ~ 1,
                                                                                  CHANGE2 < 0.5~ 0,
                                                                                  is.na(CHANGE2) == TRUE ~ -1),
                                                   POTENTIAL_COL = dplyr::case_when(POTENTIAL >= 0.5 ~ 1,
                                                                                    POTENTIAL < 0.5~ 0,
                                                                                    is.na(POTENTIAL) == TRUE ~ -1),
                                                   PLANTCOV_COL = dplyr::case_when(PLANTCOV >= 0.5 ~ 1,
                                                                                   PLANTCOV < 0.5~ 0,
                                                                                   is.na(PLANTCOV) == TRUE ~ -1),
                                                   EXPCOV_COL = dplyr::case_when(EXPCOV >= 0.5 ~ 1,
                                                                                 EXPCOV < 0.5~ 0,
                                                                                 is.na(EXPCOV) == TRUE ~ -1),
                                                   SHADECOV_COL = dplyr::case_when(SHADECOV >= 0.5 ~ 1,
                                                                                   SHADECOV < 0.5~ 0,
                                                                                   is.na(SHADECOV) == TRUE ~ -1),
                                                   MANAGEMENT_COL = dplyr::case_when(MANAGEMENT >= 0.5 ~ 1,
                                                                                     MANAGEMENT < 0.5~ 0,
                                                                                     is.na(MANAGEMENT) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
                                                            MICROSITE,
                                                            CHANGE1,
                                                            CHANGE2,
                                                            POTENTIAL,
                                                            PLANTCOV,
                                                            EXPCOV,
                                                            SHADECOV,
                                                            MANAGEMENT,
                                                            OVERALL),
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
                                     columnDefs = list(list(targets = c(22:32), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'ABUNDANCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MICROSITE', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE1', 'CHANGE1_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE2', 'CHANGE2_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POTENTIAL', 'POTENTIAL_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PLANTCOV', 'PLANTCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('EXPCOV', 'EXPCOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SHADECOV', 'SHADECOV_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('MANAGEMENT', 'MICROSITE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TARGET_MON',
                  backgroundColor = styleEqual(c(0, 1), 
                                               c("grey", "green"))) %>%
  DT::formatStyle('SUFFICIENT', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

bry_hamver_sci_dt
