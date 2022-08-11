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
str(sites_subjects)
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

# OBOJŽIVELNÍCI ----  
amp_master <- read.csv2("C:/Users/jonas.gaigr/Desktop/ADATA/_AOPK/_MONITORING/OBOJŽIVELNÍCI/amp_master_2021.csv")
write.csv(amp_master, "amp_master_202208.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

amp_master %>% 
  pull(STA_POKRVEGETACE) %>%
  na.omit()

current_year <- 2021

amp_site_eval <- function(species_name) {
  
  # FIND LEVEL
  species_target_find <- amp_master %>%
    #filter(EVL_SITECODE == evl_site) %>%
    dplyr::filter(MON_DRUH == species_name) %>%
    dplyr::mutate(
      EVL_SITECODE = dplyr::case_when(nchar(SITECODE) > 8 ~ strtrim(SITECODE, 9),
                                      TRUE ~ paste("amp", gsub("[^0-9.-]", "", SITECODE), sep = "")),
      REPRO_find = dplyr::case_when(POP_CELKPOCETVSESUBADULT > 0 | 
                                      is.na(POP_CELKPOCETVSESUBADULTREL) == FALSE |
                                      POP_POCETJUVENIL > 0 | 
                                      is.na(POP_POCETJUVENILREL) == FALSE |
                                      POP_POCETLARVA > 0 | 
                                      is.na(POP_POCETLARVAREL) == FALSE |
                                      POP_POCETMETAMORF > 0 | 
                                      is.na(POP_POCETMETAMORFREL) == FALSE |
                                      POP_CELKPOCETVSESNUSKA > 0 | 
                                      is.na(POP_CELKPOCETVSESNUSKAREL) == FALSE ~ 1),
      ABUNDANCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ne" ~ 0,
                                        POP_VYSKYTDRUH.1 == "ne" ~ 0,
                                        (POP_POCETPOHLNEURCENYADULT >= 150) |
                                          (POP_POCETSAMICE >= 150) |
                                          (POP_POCETSAMEC >= 150) |
                                          (POP_CELKPOCETVSEADULT >= 150) | 
                                          (POP_CELKPOCETVSEEXEMPLAR >= 150) ~ 3,
                                        POP_POCETPOHLNEURCENYADULTREL == "řádově stovky" |
                                          POP_POCETSAMICEREL == "řádově stovky" |
                                          POP_POCETSAMECREL == "řádově stovky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově stovky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově stovky" ~ 3,
                                        (POP_POCETPOHLNEURCENYADULT > 10 & POP_POCETPOHLNEURCENYADULT < 150) |
                                          (POP_POCETSAMICE > 10 & POP_POCETSAMICE < 150) |
                                          (POP_POCETSAMEC > 10 & POP_POCETSAMEC < 150) |
                                          (POP_CELKPOCETVSEADULT > 10 & POP_CELKPOCETVSEADULT < 150) |
                                          (POP_CELKPOCETVSEEXEMPLAR > 10 & POP_CELKPOCETVSEEXEMPLAR < 150) ~ 2,
                                        POP_POCETPOHLNEURCENYADULTREL == "cca 100" |
                                          POP_POCETSAMICEREL == "cca 100" |
                                          POP_POCETSAMECREL == "cca 100" |
                                          POP_CELKPOCETVSEADULTREL == "cca 100" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "cca 100" |
                                          POP_POCETPOHLNEURCENYADULTREL == "řádově vyšší desítky" |
                                          POP_POCETSAMICEREL == "řádově vyšší desítky" |
                                          POP_POCETSAMECREL == "řádově vyšší desítky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově vyšší desítky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově vyšší desítky" |
                                          POP_POCETPOHLNEURCENYADULTREL == "řádově nižší desítky" |
                                          POP_POCETSAMICEREL == "řádově nižší desítky" |
                                          POP_POCETSAMECREL == "řádově nižší desítky" |
                                          POP_CELKPOCETVSEADULTREL == "řádově nižší desítky" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "řádově nižší desítky" ~ 2,
                                        (POP_POCETPOHLNEURCENYADULT > 0 & POP_POCETPOHLNEURCENYADULT <= 10) |
                                          (POP_POCETSAMICE > 0 & POP_POCETSAMICE <= 10) |
                                          (POP_POCETSAMEC > 0 & POP_POCETSAMEC <= 10) |
                                          (POP_CELKPOCETVSEADULT > 0 & POP_CELKPOCETVSEADULT <= 10) |
                                          (POP_CELKPOCETVSEEXEMPLAR > 0 & POP_CELKPOCETVSEEXEMPLAR <= 10) ~ 1,
                                        POP_POCETPOHLNEURCENYADULTREL == "do 10" |
                                          POP_POCETSAMICEREL == "do 10" |
                                          POP_POCETSAMECREL == "do 10" |
                                          POP_CELKPOCETVSEADULTREL == "do 10" |
                                          POP_CELKPOCETVSEEXEMPLARREL == "do 10" ~ 1
                                        ),
      HABLOS_find = dplyr::case_when(grepl("zazemňování",
                                           VLV_CINNOSTDOPAD) ~ 0,
                                     TRUE ~ 1),
      PRED_find = dplyr::case_when(grepl("predace",
                                         VLV_CINNOSTDOPAD) ~ 0,
                                   grepl("chov ryb intensita",
                                         VLV_CINNOSTDOPAD) ~ 0,
                                   TRUE ~ 1),
      WATER_find = dplyr::case_when(grepl("chov ryb technologie",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("zaplavování pozdní",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("změny hydrografických poměrů obecně",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    TRUE ~ 1),
      DRY_find = dplyr::case_when(grepl("vysychání (mokřadního ekosystému)",
                                        VLV_CINNOSTDOPAD) ~ 0,
                                  TRUE ~ 1),
      VEG_find = dplyr::case_when(STA_POKRVEGETACE >= 25 & 
                                    STA_POKRVEGETACE <= 75 ~ 1,
                                  STA_POKRVEGETACE < 25 ~ 0,
                                  STA_POKRVEGETACE > 75 ~ 0),
      TRANSP_find = dplyr::case_when(grepl("dno", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     STA_PRUHLEDNOSTVODA >= 50 ~ 1,
                                     STA_PRUHLEDNOSTVODA < 50 ~ 0),
      BATRA_find = NA,
      THREATS_NUM_find = stringr::str_count(VLV_CINNOSTDOPAD, ","),
      TARGET_MON_find = 1,
     PRESENCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ano" ~ 1,
                                      POP_VYSKYTDRUH.1 == "ano" ~ 1,
                                      ABUNDANCE_find > 0 ~ 1,
                                      POP_VYSKYTDRUH == "ne" ~ 0,
                                      POP_VYSKYTDRUH.1 == "ne" ~ 0)
    ) %>%
    dplyr::ungroup()
  
  # SITE LEVEL
  species_target_site <- species_target_find %>%
    dplyr::group_by(SITECODE) %>%
    summarise(DRUH = unique(MON_DRUH),
              NAZEV = unique(NAZ_LOKAL)[1],
              EVL_SITECODE = unique(EVL_SITECODE),
              PRESENCE_0 = max(PRESENCE_find[ROK == current_year]),
              PRESENCE_1 = max(PRESENCE_find[ROK == current_year - 1]),
              PRESENCE_2 = max(PRESENCE_find[ROK == current_year - 2]),
              PRESENCE = dplyr::case_when(PRESENCE_0 != -Inf ~ as.numeric(PRESENCE_0),
                                          PRESENCE_1 != -Inf ~ as.numeric(PRESENCE_1),
                                          PRESENCE_2 != -Inf ~ as.numeric(PRESENCE_2)),
              POP_0 = max(ABUNDANCE_find[ROK == current_year]), 
              POP_1 = max(ABUNDANCE_find[ROK == (current_year - 1)]),
              POP_2 = max(ABUNDANCE_find[ROK == (current_year - 2)]),
              POP_A = sum(POP_0, POP_1),
              POP_B = sum(POP_0, POP_2),
              POP_C = sum(POP_1, POP_2),
              POP = dplyr::case_when(length(unique(ROK)) < 2 ~ NA_real_,
                                     PRESENCE_0 == 0 ~ 0,
                                     POP_0 >= POP_1 ~ 1,
                                     POP_0 < POP_1 ~ 0,
                                     POP_0 >= POP_2 ~ 1,
                                     POP_0 < POP_2 ~ 0,
                                     POP_1 >= POP_2 ~ 1,
                                     POP_1 < POP_2 ~ 0),
              ABUNDANCE = case_when(POP_0 != -Inf ~ as.numeric(POP_0),
                                    POP_1 != -Inf ~ as.numeric(POP_1),
                                    POP_2 != -Inf ~ as.numeric(POP_2)),
              HABLOS_0 = max(HABLOS_find[ROK == current_year]),
              HABLOS_1 = max(HABLOS_find[ROK == current_year - 1]),
              HABLOS_2 = max(HABLOS_find[ROK == current_year - 2]),
              HABLOS = case_when(HABLOS_0 != -Inf ~ as.numeric(HABLOS_0),
                                 HABLOS_1 != -Inf ~ as.numeric(HABLOS_1),
                                 HABLOS_2 != -Inf ~ as.numeric(HABLOS_2)),
              PRED_0 = max(PRED_find[ROK == current_year]),
              PRED_1 = max(PRED_find[ROK == current_year - 1]),
              PRED_2 = max(PRED_find[ROK == current_year - 2]),
              PRED = case_when(PRED_0 != -Inf ~ as.numeric(PRED_0),
                               PRED_1 != -Inf ~ as.numeric(PRED_1),
                               PRED_2 != -Inf ~ as.numeric(PRED_2)),
              TRANSP_0 = min(TRANSP_find[ROK == current_year]),
              TRANSP_1 = min(TRANSP_find[ROK == current_year - 1]),
              TRANSP_2 = min(TRANSP_find[ROK == current_year - 2]),
              TRANSP = case_when(TRANSP_0 != Inf ~ as.numeric(TRANSP_0),
                                 TRANSP_1 != Inf ~ as.numeric(TRANSP_1),
                                 TRANSP_2 != Inf ~ as.numeric(TRANSP_2)),
              REPRO_0 = max(REPRO_find[ROK == current_year]),
              REPRO_1 = max(REPRO_find[ROK == current_year - 1]),
              REPRO_2 = max(REPRO_find[ROK == current_year - 2]),
              REPRO = dplyr::case_when(REPRO_0 != -Inf ~ as.numeric(REPRO_0),
                                       REPRO_1 != -Inf ~ as.numeric(REPRO_1),
                                       REPRO_2 != -Inf ~ as.numeric(REPRO_2)),
              WATER_0 = max(WATER_find[ROK == current_year]),
              WATER_1 = max(WATER_find[ROK == current_year - 1]),
              WATER_2 = max(WATER_find[ROK == current_year - 2]),
              WATER = dplyr::case_when(WATER_0 != -Inf ~ as.numeric(WATER_0),
                                       WATER_1 != -Inf ~ as.numeric(WATER_1),
                                       WATER_2 != -Inf ~ as.numeric(WATER_2)),
              DRY_0 = max(DRY_find[ROK == current_year]),
              DRY_1 = max(DRY_find[ROK == current_year - 1]),
              DRY_2 = max(DRY_find[ROK == current_year - 2]),
              DRY = dplyr::case_when(DRY_0 != -Inf ~ as.numeric(DRY_0),
                                     DRY_1 != -Inf ~ as.numeric(DRY_1),
                                     DRY_2 != -Inf ~ as.numeric(DRY_2)),
              VEG_0 = max(VEG_find[ROK == current_year]),
              VEG_1 = max(VEG_find[ROK == current_year - 1]),
              VEG_2 = max(VEG_find[ROK == current_year - 2]),
              VEG = dplyr::case_when(VEG_0 != -Inf ~ as.numeric(VEG_0),
                                     VEG_1 != -Inf ~ as.numeric(VEG_1),
                                     VEG_2 != -Inf ~ as.numeric(VEG_2)),
              NAZEV_LOKAL = unique(NAZ_LOKAL)[1]) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(
      OVERALL_sum = sum(PRESENCE,
                        POP,
                        HABLOS,
                        PRED,
                        TRANSP,
                        REPRO,
                        WATER,
                        DRY,
                        VEG)
      )%>%
    dplyr::mutate(
      OVERALL = dplyr::case_when(OVERALL_sum == 9 ~ 1,
                                 OVERALL_sum < 9 &
                                   OVERALL_sum >=5 &
                                   PRESENCE == 1 ~ 0.5,
                                 OVERALL_sum < 5 |
                                   PRESENCE == 0 ~ 0),
      SUFFICIENT = dplyr::case_when(OVERALL == 1 ~ 1,
                                    OVERALL < 1 ~ 0),
          ) %>%
    dplyr::select(EVL_SITECODE,
                  SITECODE,
                  NAZEV,
                  DRUH,
                  PRESENCE,
                  POP,
                  HABLOS,
                  PRED,
                  TRANSP,
                  REPRO,
                  WATER,
                  DRY,
                  VEG,
                  OVERALL,
                  SUFFICIENT)

  result <- species_target_site 
  
  result
  
}

amp_sci_eval <- function(species_name) {
  
  species_target_sci <- amp_site_eval(species_name) %>%
    dplyr::group_by(EVL_SITECODE) %>%
    dplyr::summarise(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) == 9 ~ as.character(find_evl_CODE_TO_NAME(unique(EVL_SITECODE))),
                               TRUE ~ as.character("HOVNO")),
      DRUH = species_name,
      PRESENCE = mean(na.omit(PRESENCE)),
      POP = mean(na.omit(POP)),
      HABLOS = mean(na.omit(HABLOS)),
      PRED = mean(na.omit(PRED)),
      TRANSP = mean(na.omit(TRANSP)),
      REPRO = mean(na.omit(REPRO)),
      WATER = mean(na.omit(WATER)),
      DRY = mean(na.omit(DRY)),
      VEG = mean(na.omit(VEG)),
      OVERALL = mean(na.omit(OVERALL)),
      SUFFICIENT_SITES = sum(SUFFICIENT, na.rm = TRUE),
      TOTAL_SITES = n()
    ) %>%
    group_by(EVL_SITECODE) %>%
    dplyr::mutate(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) == 9 ~ find_evl_CODE_TO_NAME(unique(EVL_SITECODE)),
                               TRUE ~ "HOVNO")
      )
  
  result <- species_target_sci
  
  result
  
}

# RESULTS ----
bombom_site_eval <- amp_site_eval("Bombina bombina")
bomvar_site_eval <- amp_site_eval("Bombina variegata")
tricri_site_eval <- amp_site_eval("Triturus cristatus")
tricar_site_eval <- amp_site_eval("Triturus carnifex")
lismon_site_eval <- amp_site_eval("Lissotriton montandoni")
amp_site_results <- dplyr::bind_rows(bombom_site_eval,
                                     bomvar_site_eval,
                                     tricri_site_eval,
                                     tricar_site_eval,
                                     lismon_site_eval)
write.csv2(amp_site_results, 
           "amp_site_results.csv",
           row.names = FALSE)

bombom_sci_eval <- amp_sci_eval("Bombina bombina")
bomvar_sci_eval <- amp_sci_eval("Bombina variegata")
tricri_sci_eval <- amp_sci_eval("Triturus cristatus")
tricar_sci_eval <- amp_sci_eval("Triturus carnifex")
lismon_sci_eval <- amp_sci_eval("Lissotriton montandoni")
amp_sci_results <- dplyr::bind_rows(bombom_sci_eval,
                                    bomvar_sci_eval,
                                    tricri_sci_eval,
                                    tricar_sci_eval,
                                    lismon_sci_eval)
write.csv2(amp_sci_results, 
           "amp_sci_results.csv",
           row.names = FALSE)

write.csv(amp_site_results, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_amp_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(amp_sci_results, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_amp_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

bombomtest <- amp_sci_eval("Bombina bombina")

bomtest %>%
  mutate(SUMA = sum(PRESENCE)) %>%
  dplyr::select(NAZEV, SUMA) %>%
  arrange(-SUMA)
amp_master %>%
  pull(MON_DRUH) %>%
  unique
bomtest <- species_read %>%
  filter(DRUH == "Bombina bombina") %>%
  filter(STRUKT_POZN != "") %>%
  select(STRUKT_POZN)
bombom_site_eval %>%
  filter(SITECODE == "amp103") %>%
  group_by(EVL_SITECODE) %>%
  mutate(chch = nchar(EVL_SITECODE)) %>%
  mutate(EVL_NAZEV = unique(NAZEV)[1]) %>%
  as.data.frame()
