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
    # Redukce data na měsíc
    MONTH = substring(DATE, 5, 6),
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
# FUNKCE CHI ----
current_year <- 2021

mam_chi_site_eval <- function(species_code, evl_site) {
  
  # FIND LEVEL
  species_target_find <- species %>%
    dplyr::filter(SITECODE == evl_site) %>%
    dplyr::filter(DRUH == species_code) %>%
    dplyr::filter(PRESNOST < 1500) %>%
    dplyr::mutate(
      PRESENCE_find = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0))
  
  # SITE LEVEL
  species_target_sites_all <- species_target_find %>%
    dplyr::filter(YEAR > current_year - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::summarise(
      SITECODE = unique(SITECODE),
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = unique(DRUH),
      FEATURE_CODE = find_N2K_feature_code(species_code),
      PRESENCE = dplyr::case_when(max(PRESENCE_find[YEAR == current_year]) != -Inf ~ max(PRESENCE_find[YEAR == current_year]),
                                  TRUE ~ NA_real_)
           ) %>%
    dplyr::ungroup() %>%
    select(SITECODE, 
           NAZEV,
           LOKALITA, 
           DRUH, 
           FEATURE_CODE,
           PRESENCE)
  
  species_target_sites_winter <- species_target_find %>%
    dplyr::filter(YEAR > current_year - 6) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::summarise(
      SITECODE = unique(SITECODE),
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      DRUH = unique(DRUH),
      FEATURE_CODE = find_N2K_feature_code(species_code),
      PRESENCE_WINTER = dplyr::case_when(max(PRESENCE_find[(YEAR == current_year & MONTH < 5) |
                                                             (YEAR == current_year - 1 & MONTH > 9)]) != -Inf ~ max(PRESENCE_find[(YEAR == current_year & MONTH < 5) |
                                                                                                                                     (YEAR == current_year - 1 & MONTH > 9)]),
                                  TRUE ~ NA_real_),
      POP_0 = max(POCET[(YEAR == current_year & MONTH < 5) |
                          (YEAR == current_year - 1 & MONTH > 9)]), 
      POP_1 = max(POCET[(YEAR == current_year - 1 & MONTH < 5) |
                          (YEAR == current_year - 2 & MONTH > 9)]),
      POP_2 = max(POCET[(YEAR == current_year - 2 & MONTH < 5) |
                          (YEAR == current_year - 3 & MONTH > 9)]),
      POP_3 = max(POCET[(YEAR == current_year - 3& MONTH < 5) |
                          (YEAR == current_year - 4 & MONTH > 9)]),
      POP_mean = mean(na.omit(POP_1, POP_2, POP_3)),
      ABUNDANCE_SITE = POP_0,
      CHANGE = dplyr::case_when(FEATURE_CODE == 1308 &
                                  POP_0/POP_mean >= 0.67 ~ 1,
                                FEATURE_CODE == 1308 & 
                                  POP_0/POP_mean < 0.67 ~ 0,
                                DRUH == "Myotis emarginatus " &
                                  POP_0/POP_mean >= 0.5 ~ 1,
                                DRUH == "Myotis emarginatus " & 
                                  POP_0/POP_mean < 0.5 ~ 0,
                                FEATURE_CODE == 1324 &
                                  POP_0/POP_mean >= 0.5 ~ 1,
                                FEATURE_CODE == 1324 & 
                                  POP_0/POP_mean < 0.5 ~ 0,
                                FEATURE_CODE == 1303 &
                                  POP_0/POP_mean >= 0.5 ~ 1,
                                FEATURE_CODE == 1303 &
                                  POP_0/POP_mean < 0.5 ~ 0,
                                DRUH == "Myotis bechsteinii" ~ NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    replace(. == -Inf, NA) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(POP_0_ALL = sum(POP_0, na.rm = TRUE),
                  POP_1_ALL = sum(POP_1, na.rm = TRUE),
                  POP_2_ALL = sum(POP_2, na.rm = TRUE),
                  POP_3_ALL = sum(POP_3, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    select(SITECODE, 
           NAZEV,
           LOKALITA, 
           DRUH, 
           FEATURE_CODE,
           PRESENCE_WINTER,
           ABUNDANCE_SITE,
           POP_0_ALL,
           POP_1_ALL,
           POP_2_ALL,
           POP_3_ALL,
           CHANGE)
  
  last_find_site <- species_target_find %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::filter(PRESENCE_find == 1) %>%
    dplyr::summarise(SITECODE = evl_site,
                     LAST_FIND = max(DATE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SITECODE, LOKALITA, LAST_FIND)
  
  result <- species_target_sites_all %>%
    dplyr::full_join(.,
                     species_target_sites_winter,
                     by = c("SITECODE", 
                            "NAZEV",
                            "LOKALITA",
                            "DRUH",
                            "FEATURE_CODE"
                            )
                     ) %>%
    dplyr::full_join(.,
                     last_find_site, 
                     by = c("SITECODE", "LOKALITA")) %>%
    dplyr::group_by(LOKALITA) %>%
    dplyr::mutate(
      OVERALL = sum(PRESENCE,
                    CHANGE,
                    na.rm = TRUE)
    )
  
  result <- replace(result, result == -Inf, NA)
  
  result
  
}

mam_chi_sci_eval <- function(species_code, sci_code) {
  
  # SCI LEVEL
  species_target_sci <- results_site_chi %>%
    dplyr::filter(FEATURE_CODE == species_code)  %>%
    dplyr::filter(SITECODE == sci_code)
  
  if(nrow(species_target_sci) > 0) {
    result <- species_target_sci %>%
      dplyr::group_by(SITECODE) %>%
      dplyr::mutate(
        POP_mean = mean(na.omit(POP_1_ALL, 
                                POP_2_ALL, 
                                POP_3_ALL))
        ) %>%
      dplyr::summarise(
        NAZEV = unique(NAZEV),
        FEATURE_CODE = unique(FEATURE_CODE),
        DRUH = unique(DRUH),
        PRESENCE = mean(na.omit(PRESENCE)),
        PRESENCE_WINTER = mean(na.omit(PRESENCE_WINTER)),
        ABUNDANCE = sum(ABUNDANCE_SITE, na.rm = TRUE),
        CHANGE_SITES = mean(na.omit(CHANGE)),
        POP_mean = unique(POP_mean),
        LAST_FIND = max(na.omit(LAST_FIND)),
        OVERALL = mean(na.omit(OVERALL))
        ) %>%
      dplyr::mutate(
        CHANGE_NUM = as.numeric(ABUNDANCE/POP_mean),
        CHANGE_ALL = dplyr::case_when(CHANGE_NUM == Inf ~ 1,
                                      FEATURE_CODE == 1308 &
                                        CHANGE_NUM >= 0.67 ~ 1,
                                      FEATURE_CODE == 1308 & 
                                        CHANGE_NUM < 0.67 ~ 0,
                                      DRUH == "Myotis emarginatus" &
                                        CHANGE_NUM >= 0.5 ~ 1,
                                      DRUH == "Myotis emarginatus" & 
                                        CHANGE_NUM < 0.5 ~ 0,
                                      FEATURE_CODE == 1324 &
                                        CHANGE_NUM >= 0.5 ~ 1,
                                      FEATURE_CODE == 1324 & 
                                        CHANGE_NUM < 0.5 ~ 0,
                                      FEATURE_CODE == 1303 &
                                        CHANGE_NUM >= 0.5 ~ 1,
                                      FEATURE_CODE == 1303 &
                                        CHANGE_NUM < 0.5 ~ 0,
                                      DRUH == "Myotis bechsteinii" ~ NA_real_),
        SUFFICIENT = dplyr::case_when(DRUH == "Myotis bechsteinii" &
                                        PRESENCE == 1 ~ 1,
                                      DRUH == "Myotis bechsteinii" &
                                        PRESENCE == 0 ~ 0,
                                      PRESENCE == 0 ~ 0,
                                      PRESENCE == 1 &
                                        CHANGE_ALL == 1 ~ 1,
                                      PRESENCE > 0 &
                                        CHANGE_ALL == 0 ~ 0.5)
      ) %>%
      dplyr::select(SITECODE,
                    NAZEV,
                    FEATURE_CODE,
                    DRUH,
                    PRESENCE,
                    PRESENCE_WINTER,
                    ABUNDANCE,
                    CHANGE_SITES,
                    CHANGE_NUM,
                    CHANGE_ALL,
                    OVERALL,
                    SUFFICIENT,
                    LAST_FIND)
  } else {
    result <- tibble(
      SITECODE = sci_code,
      NAZEV = find_evl_CODE_TO_NAME(sci_code),
      FEATURE_CODE = species_code,
      DRUH = find_N2K_name(species_code),
      PRESENCE = NA,
      ABUNDANCE = NA,
      CHANGE_SITES = NA,
      CHANGE_ALL = NA,
      OVERALL = NA,
      SUFFICIENT = NA,
      LAST_FIND = NA)
  }
  
  result <- result %>%
    dplyr::distinct()
    
}


# RESULTS MAMMALS ----
# |-chi_1_sites ----
sites_chi <- sites_subjects %>%
  filter(feature_code == "1324" |
           feature_code == "1308" |
           feature_code == "1323" |
           feature_code == "1321" |
           feature_code == "1303")

hu_chi_1 <- mam_chi_site_eval(sites_chi[1,7], sites_chi[1,1])
results_site_chi <- matrix(NA, 1, ncol(hu_chi_1)) %>% 
  dplyr::as_tibble()
colnames(results_site_chi) <- colnames(hu_chi_1)

for(i in 1:nrow(sites_chi)) {
  results_site_chi <- dplyr::bind_rows(results_site_chi, 
                                          as.data.frame(mam_chi_site_eval(sites_chi[i,7], sites_chi[i,1])))
}
results_site_chi <- results_site_chi %>%
  filter(is.na(DRUH) == FALSE)
results_site_chi <- results_site_chi[c(2:nrow(results_site_chi)),]
write.csv(results_site_chi, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_chi_site_2021.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_site_chi, 
          "C:/Users/jonas.gaigr/Desktop/results_chi_site_2021.csv",
          row.names = FALSE,
          fileEncoding = "Windows-1250")


# |-chi_1_sci ----
hu_chi_1 <- mam_chi_sci_eval(sites_chi[1,5], sites_chi[1,1])
results_sci_chi <- matrix(NA, 1, ncol(hu_chi_1)) %>% 
  dplyr::as_tibble()
colnames(results_sci_chi) <- colnames(hu_chi_1)

for(i in 1:nrow(sites_chi)) {
  results_sci_chi <- dplyr::bind_rows(results_sci_chi, 
                                      as.data.frame(mam_chi_sci_eval(sites_chi[i,5], 
                                                                     sites_chi[i,1])))
}

results_sci_chi <- results_sci_chi[c(2:nrow(results_sci_chi)),]

write.csv(results_sci_chi, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_chi_sci_2021.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_sci_chi, 
           "C:/Users/jonas.gaigr/Desktop/results_chi_sci_2021.csv",
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# DATATABLE ----
library(DT)
# |-Myotis myotis----
chi_myomyo_sci_dt <- DT::datatable(results_sci_chi %>%
                                     dplyr::filter(DRUH == "Myotis myotis") %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     CHANGE_ALL)) == TRUE ~ 0,
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
                                                   CHANGE_COL = dplyr::case_when(CHANGE_ALL == 1 ~ 1,
                                                                                 CHANGE_ALL == 0 ~ 0,
                                                                                 is.na(CHANGE_ALL) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
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
                                     columnDefs = list(list(targets = c(14:16), visible = FALSE))),
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
  DT::formatStyle('PRESENCE_WINTER', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_SITES', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_NUM', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_ALL', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

chi_myomyo_sci_dt

# |-Myotis emarginatus----
chi_myoema_sci_dt <- DT::datatable(results_sci_chi %>%
                                     dplyr::filter(DRUH == "Myotis emarginatus") %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     CHANGE_ALL)) == TRUE ~ 0,
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
                                                   CHANGE_COL = dplyr::case_when(CHANGE_ALL == 1 ~ 1,
                                                                                 CHANGE_ALL == 0 ~ 0,
                                                                                 is.na(CHANGE_ALL) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
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
                                     columnDefs = list(list(targets = c(14:16), visible = FALSE))),
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
  DT::formatStyle('PRESENCE_WINTER', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_SITES', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_NUM', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_ALL', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

chi_myoema_sci_dt

# |-Myotis bechsteinii----
chi_myobec_sci_dt <- DT::datatable(results_sci_chi %>%
                                     dplyr::filter(DRUH == "Myotis bechsteinii") %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE)) == TRUE ~ 0,
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
                                                   CHANGE_COL = dplyr::case_when(CHANGE_ALL == 1 ~ 1,
                                                                                 CHANGE_ALL == 0 ~ 0,
                                                                                 is.na(CHANGE_ALL) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
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
                                     columnDefs = list(list(targets = c(14:16), visible = FALSE))),
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
  DT::formatStyle('PRESENCE_WINTER', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_SITES', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_NUM', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_ALL', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

chi_myobec_sci_dt

# |-Barbastella barbastellus ----
chi_barbar_sci_dt <- DT::datatable(results_sci_chi %>%
                                     dplyr::filter(DRUH == "Barbastella barbastellus") %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     CHANGE_ALL)) == TRUE ~ 0,
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
                                                   CHANGE_COL = dplyr::case_when(CHANGE_ALL == 1 ~ 1,
                                                                                 CHANGE_ALL == 0 ~ 0,
                                                                                 is.na(CHANGE_ALL) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
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
                                     columnDefs = list(list(targets = c(14:16), visible = FALSE))),
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
  DT::formatStyle('PRESENCE_WINTER', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_SITES', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_NUM', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_ALL', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

chi_barbar_sci_dt

# |-Rhinolophus hipposideros ----
chi_rhihip_sci_dt <- DT::datatable(results_sci_chi %>%
                                     dplyr::filter(DRUH == "Rhinolophus hipposideros") %>%
                                     dplyr::group_by(SITECODE) %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE,
                                                                                     CHANGE_ALL)) == TRUE ~ 0,
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
                                                   CHANGE_COL = dplyr::case_when(CHANGE_ALL == 1 ~ 1,
                                                                                 CHANGE_ALL == 0 ~ 0,
                                                                                 is.na(CHANGE_ALL) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE,
                                                            ABUNDANCE,
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
                                     columnDefs = list(list(targets = c(14:16), visible = FALSE))),
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
  DT::formatStyle('PRESENCE_WINTER', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ABUNDANCE', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_SITES', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_NUM', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHANGE_ALL', 'CHANGE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT','ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

chi_rhihip_sci_dt

# |-All species ---- 
chi_myomyo_sci_dt
chi_myoema_sci_dt
chi_myobec_sci_dt
chi_barbar_sci_dt
chi_rhihip_sci_dt


species %>%
  filter(DRUH == "Myotis myotis") %>%
  filter(EVL == "CZ0724089: Beskydy") %>%
  arrange(-as.numeric(YEAR))
  summarise(SITE = unique(SITECODE),
            COUNT = sum(POCET, na.rm = TRUE))
