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

# Mechorosty ----
nvp_buxvir_sci_eval <- function(sci_code) {
  
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
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::arrange(desc(DATE)) %>%
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
                     by = "SITECODE")
  
  result
  
}

# RESULTS ----
# |-Buxbaumia viridis ----
sites_buxvir <- sites_subjects %>%
  filter(feature_code == 1386)
hu_buxvir <- nvp_buxvir_sci_eval(sites_buxvir[1,1])
results_sci_buxvir <- matrix(NA, 1, ncol(hu_buxvir)) %>% 
  dplyr::as_tibble()
colnames(results_sci_buxvir) <- colnames(hu_buxvir)

nvp_buxvir_sci_eval("CZ0714081")

for(i in 1:nrow(sites_buxvir)) {
  results_sci_buxvir <- dplyr::bind_rows(results_sci_buxvir, 
                                         as.data.frame(nvp_buxvir_sci_eval(sites_buxvir[i,1])))
}

# |-Dicranum viridae ----
sites_dicvir <- sites_subjects %>%
  filter(feature_code == 1386)
hu_dicvir <- nvp_dicvir_sci_eval(sites_dicvir[1,7], sites_dicvir[1,1])
results_sci_dicvir <- matrix(NA, 1, ncol(hu_dicvir)) %>% 
  dplyr::as_tibble()
colnames(results_sci_dicvir) <- colnames(hu_dicvir)

# |-Hamatocaulis vernicosus ----
sites_hamver <- sites_subjects %>%
  filter(feature_code == 6216)
hu_dicvir <- nvp_hamver_sci_eval(sites_hamver[1,7], sites_hamver[1,1])
results_sci_dicvir <- matrix(NA, 1, ncol(hu_dicvir)) %>% 
  dplyr::as_tibble()
colnames(results_sci_dicvir) <- colnames(hu_dicvir)
