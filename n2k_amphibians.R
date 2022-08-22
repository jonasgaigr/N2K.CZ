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
amp_2021 <- read.csv2("C:/Users/jonas.gaigr/Desktop/sledovani_evl_2021.csv") %>%
  mutate(OBJECTID = as.character(OBJECTID),
         #ABI_TEPLOTA = as.character(ABI_TEPLOTA),
         #STA_STAVVODAPERIODTUN = as.character(STA_STAVVODAPERIODTUN),
         #STA_STAVVODALITORAL = as.character(STA_STAVVODALITORAL),
         #STA_PRUHLEDNOSTVODA = as.character(STA_PRUHLEDNOSTVODA),
         #STA_POKRVEGETACE = as.character(STA_POKRVEGETACE),
         #POP_CELKPOCETVSEADULT = as.character(POP_CELKPOCETVSEADULT),
         POP_POCETSAMEC = as.character(POP_POCETSAMEC),
         #POP_POCETSAMICE = as.character(POP_POCETSAMICE),
         #POP_POCETPOHLNEURCENYADULT = as.character(POP_POCETPOHLNEURCENYADULT),
         #POP_POCETMRTVYJEDINEC = as.character(POP_POCETMRTVYJEDINEC),
         #POP_CELKPOCETVSESUBADULT = as.character(POP_CELKPOCETVSESUBADULT),
         #POP_POCETJUVENIL = as.character(POP_POCETJUVENIL),
         #POP_POCETLARVA = as.character(POP_POCETLARVA),
         #POP_POCETMETAMORF = as.character(POP_POCETMETAMORF),
         POP_CELKPOCETVSESNUSKA = as.character(POP_CELKPOCETVSESNUSKA),
         #POP_PLOCHASNUSKA = as.character(POP_PLOCHASNUSKA)
         )

amp_master <- bind_rows(amp_master, amp_2021)
#amp_master <- full_join(amp_master, amp_2021)
amp_master <- amp_master %>%
  mutate(KOD_LOKALITA = case_when(KOD_LOK == "amp216" ~ "CZ0723412",
                                  KOD_LOK == "amp222" ~ "CZ0724089_9",
                                  KOD_LOK == "amp231" ~ "CZ0724089_19",
                                  KOD_LOK == "amp185" ~ "CZ0623345",
                                  KOD_LOK == "amp101" ~ "CZ0423006",
                                  KOD_LOK == "amp71" ~ "CZ0323158",
                                  KOD_LOK == "amp59" ~ "CZ0323144",
                                  KOD_LOK == "amp15" ~ "CZ0213790",
                                  KOD_LOK == "amp102" ~ "CZ0423215",
                                  (KOD_LOK != "" | 
                                    is.na(KOD_LOK)) == FALSE &
                                    (KOD_LOKALITA == "" |
                                    is.na(KOD_LOKALITA) == TRUE) ~ KOD_LOK,
                                  TRUE ~ KOD_LOKALITA)) %>%
  mutate(SITECODE = KOD_LOKALITA)

write.csv2(amp_master, "amp_all_202208.csv",
           row.names = FALSE)

current_year <- 2021

#|-Bombina bombina ----
amp_bombom_site_eval <- function(species_name) {
  
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
                                        POP_POCETSAMEC >= 150 ~ 3,
                                        POP_POCETSAMECREL == "řádově stovky" ~ 3,
                                        (POP_POCETSAMEC > 10 & POP_POCETSAMEC < 150) ~ 2,
                                        POP_POCETSAMECREL == "cca 100" |
                                          POP_POCETSAMECREL == "řádově vyšší desítky" |
                                          POP_POCETSAMECREL == "řádově nižší desítky" ~ 2,
                                        (POP_POCETSAMEC > 0 & POP_POCETSAMEC <= 10) ~ 1,
                                        POP_POCETSAMECREL == "do 10" ~ 1,
                                        TRUE ~ 0
                                        ),
      HABLOS_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                     STA_STAVVODALITORAL < 10 ~ 0,
                                     STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                     STA_STAVVODALITORAL >= 10 ~ 1,
                                     TRUE ~ 1),
      PRED_find = dplyr::case_when(STA_KACHNALOV == "ano" ~ 0,
                                   STA_ZOOPLANKTON == "ne" ~ 0,
                                   STA_KACHNALOV == "ne" ~ 1,
                                   STA_ZOOPLANKTON == "ano" ~ 1,
                                   TRUE ~ 1),
      WATER_find = dplyr::case_when(grepl("regulování vodní hladiny",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("chov ryb technologie",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("zaplavování pozdní",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("změny hydrografických poměrů obecně",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    TRUE ~ 1),
      DRY_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                  STA_STAVVODALITORAL < 10 ~ 0,
                                  STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                  STA_STAVVODALITORAL >= 10 ~ 1,
                                  grepl("vysychání (mokřadního ekosystému)",
                                        VLV_CINNOSTDOPAD) ~ 0,
                                  TRUE ~ 1),
      VEG_find = dplyr::case_when(STA_POKRVEGETACE >= 25 & 
                                    STA_POKRVEGETACE <= 75 ~ 1,
                                  STA_POKRVEGETACE < 25 ~ 0,
                                  STA_POKRVEGETACE > 75 ~ 0),
      TRANSP_find = dplyr::case_when(grepl("dno", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     grepl("", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     is.na(STA_PRUHLEDNOSTVODAPOZN) == TRUE ~ 1,
                                     STA_PRUHLEDNOSTVODA >= 50 ~ 1,
                                     STA_PRUHLEDNOSTVODA < 50 ~ 0,
                                     TRUE ~ 1),
      BATRA_find = NA,
      ZASTIN_find = dplyr::case_when(STA_ZASTINENI > 50 ~ 0,
                                     STA_ZASTINENI <= 50 ~ 1,
                                     TRUE ~ 1),
      THREATS_NUM_find = stringr::str_count(VLV_CINNOSTDOPAD, ","),
      TARGET_MON_find = 1,
      PRESENCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ano" ~ 1,
                                       POP_VYSKYTDRUH.1 == "ano" ~ 1,
                                       POP_DRUHANONE == "ano" ~ 1,
                                       ABUNDANCE_find > 0 ~ 1,
                                       POP_VYSKYTDRUH == "ne" ~ 0,
                                       POP_VYSKYTDRUH.1 == "ne" ~ 0,
                                       POP_DRUHANONE == "ne" ~ 0)
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
              POP = dplyr::case_when(PRESENCE_0 == 0 ~ 0,
                                     POP_0 >= POP_1 ~ 1,
                                     POP_0 < POP_1 ~ 0,
                                     POP_0 >= POP_2 ~ 1,
                                     POP_0 < POP_2 ~ 0,
                                     POP_1 >= POP_2 ~ 1,
                                     POP_1 < POP_2 ~ 0,
                                     length(unique(ROK)) < 2 ~ NA_real_),
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
              CHYTRI = 1,
              ZASTIN_0 = max(ZASTIN_find[ROK == current_year]),
              ZASTIN_1 = max(ZASTIN_find[ROK == current_year - 1]),
              ZASTIN_2 = max(ZASTIN_find[ROK == current_year - 2]),
              ZASTIN = dplyr::case_when(ZASTIN_0 != -Inf ~ as.numeric(ZASTIN_0),
                                        ZASTIN_1 != -Inf ~ as.numeric(ZASTIN_1),
                                        ZASTIN_2 != -Inf ~ as.numeric(ZASTIN_2)),
              NAZEV_LOKAL = unique(NAZ_LOKAL)[1]) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(
      OVERALL_sum = sum(PRESENCE,
                        POP,
                        REPRO,
                        HABLOS,
                        PRED,
                        WATER,
                        DRY,
                        VEG,
                        TRANSP,
                        CHYTRI,
                        ZASTIN,
                        na.rm = TRUE)
      )%>%
    dplyr::mutate(
      OVERALL = dplyr::case_when(OVERALL_sum == 11 ~ 1,
                                 OVERALL_sum < 11 &
                                   OVERALL_sum >= 8 &
                                   PRESENCE == 1 ~ 0.5,
                                 OVERALL_sum < 8 |
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
                  REPRO,
                  HABLOS,
                  PRED,
                  WATER,
                  DRY,
                  VEG,
                  TRANSP,
                  CHYTRI,
                  ZASTIN,
                  OVERALL,
                  SUFFICIENT) %>%
    dplyr::rename(PRESENCE_P1a = PRESENCE,
                  POP_P1b = POP,
                  REPRO_P2 = REPRO,
                  HABLOS_P3 = HABLOS,
                  PRED_P4 = PRED,
                  WATER_P5 = WATER,
                  DRY_P6 = DRY,
                  VEG_P7 = VEG,
                  TRANSP_P8 = TRANSP,
                  CHYTRI_P9 = CHYTRI,
                  ZASTIN_P10 = ZASTIN)

  result <- species_target_site 
  
  result
  
}

amp_bombom_sci_eval <- function(species_name) {
  
  species_target_sci <- amp_bombom_site_eval(species_name) %>%
    dplyr::group_by(EVL_SITECODE) %>%
    dplyr::summarise(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) != 9 ~ as.character(unique(NAZEV)[1]),
                               nchar(unique(EVL_SITECODE)) == 9 ~ as.character(find_evl_CODE_TO_NAME(unique(EVL_SITECODE))),
                               TRUE ~ as.character("HOVNO")),
      DRUH = species_name,
      PRESENCE_P1a = mean(na.omit(PRESENCE_P1a)),
      POP_P1b = mean(na.omit(POP_P1b)),
      REPRO_P2 = mean(na.omit(REPRO_P2)),
      HABLOS_P3 = mean(na.omit(HABLOS_P3)),
      PRED_P4 = mean(na.omit(PRED_P4)),
      WATER_P5 = mean(na.omit(WATER_P5)),
      DRY_P6 = mean(na.omit(DRY_P6)),
      VEG_P7 = mean(na.omit(VEG_P7)),
      TRANSP_P8 = mean(na.omit(TRANSP_P8)),
      CHYTRI_P9 = mean(na.omit(CHYTRI_P9)),
      ZASTIN_P10 = mean(na.omit(ZASTIN_P10)),
      # CELKOVE HODNOCENI LOKALITY
      OVERALL = mean(na.omit(OVERALL)),
      # POCET LOKALIT V DOSTATECNEM STAVU
      SUFFICIENT_SITES = sum(SUFFICIENT, na.rm = TRUE),
      # CELKOVY POCET LOKALIT
      TOTAL_SITES = n(),
      SUFFICIENT_PERC = SUFFICIENT_SITES/TOTAL_SITES
    )
  
  result <- species_target_sci
  
  result
  
}


#|-Bombina variegata ----
amp_bomvar_site_eval <- function(species_name) {
  
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
                                          POP_CELKPOCETVSEEXEMPLARREL == "do 10" ~ 1,
                                        TRUE ~ 1
      ),
      HABLOS_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                     STA_STAVVODALITORAL < 10 ~ 0,
                                     STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                     STA_STAVVODALITORAL >= 10 ~ 1,
                                     TRUE ~ 1),
      PRED_find = dplyr::case_when(STA_KACHNALOV == "ano" ~ 0,
                                   STA_ZOOPLANKTON == "ne" ~ 0,
                                   STA_KACHNALOV == "ne" ~ 1,
                                   STA_ZOOPLANKTON == "ano" ~ 1,
                                   TRUE ~ 1),
      WATER_find = dplyr::case_when(grepl("regulování vodní hladiny",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("chov ryb technologie",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("zaplavování pozdní",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("změny hydrografických poměrů obecně",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    TRUE ~ 1),
      DRY_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                  STA_STAVVODALITORAL < 10 ~ 0,
                                  STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                  STA_STAVVODALITORAL >= 10 ~ 1,
                                  grepl("vysychání (mokřadního ekosystému)",
                                        VLV_CINNOSTDOPAD) ~ 0,
                                  TRUE ~ 1),
      VEG_find = dplyr::case_when(STA_POKRVEGETACE <= 50 ~ 1,
                                  STA_POKRVEGETACE > 50 ~ 0),
      TRANSP_find = dplyr::case_when(grepl("dno", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     grepl("", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     is.na(STA_PRUHLEDNOSTVODAPOZN) == TRUE ~ 1,
                                     STA_PRUHLEDNOSTVODA >= 50 ~ 1,
                                     STA_PRUHLEDNOSTVODA < 50 ~ 0,
                                     TRUE ~ 1),
      BATRA_find = 1,
      ZASTIN_find = dplyr::case_when(STA_ZASTINENI > 50 ~ 0,
                                     STA_ZASTINENI <= 50 ~ 1),
      THREATS_NUM_find = stringr::str_count(VLV_CINNOSTDOPAD, ","),
      TARGET_MON_find = 1,
      PRESENCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ano" ~ 1,
                                       POP_VYSKYTDRUH.1 == "ano" ~ 1,
                                       POP_DRUHANONE == "ano" ~ 1,
                                       ABUNDANCE_find > 0 ~ 1,
                                       POP_VYSKYTDRUH == "ne" ~ 0,
                                       POP_VYSKYTDRUH.1 == "ne" ~ 0,
                                       POP_DRUHANONE == "ne" ~ 0)
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
              POP = dplyr::case_when(PRESENCE_0 == 0 ~ 0,
                                     POP_0 >= POP_1 ~ 1,
                                     POP_0 < POP_1 ~ 0,
                                     POP_0 >= POP_2 ~ 1,
                                     POP_0 < POP_2 ~ 0,
                                     POP_1 >= POP_2 ~ 1,
                                     POP_1 < POP_2 ~ 0,
                                     length(unique(ROK)) < 2 ~ NA_real_),
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
              CHYTRI = 1,
              ZASTIN_0 = max(ZASTIN_find[ROK == current_year]),
              ZASTIN_1 = max(ZASTIN_find[ROK == current_year - 1]),
              ZASTIN_2 = max(ZASTIN_find[ROK == current_year - 2]),
              ZASTIN = dplyr::case_when(ZASTIN_0 != -Inf ~ as.numeric(ZASTIN_0),
                                        ZASTIN_1 != -Inf ~ as.numeric(ZASTIN_1),
                                        ZASTIN_2 != -Inf ~ as.numeric(ZASTIN_2)),
              NAZEV_LOKAL = unique(NAZ_LOKAL)[1]) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(
      OVERALL_sum = sum(PRESENCE,
                        POP,
                        HABLOS,
                        PRED,
                        REPRO,
                        DRY,
                        VEG,
                        CHYTRI,
                        ZASTIN,
                        na.rm = TRUE)
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
                  REPRO,
                  DRY,
                  VEG,
                  CHYTRI,
                  ZASTIN,
                  OVERALL,
                  SUFFICIENT) %>%
    dplyr::rename(PRESENCE_P1a = PRESENCE,
                  POP_P1b = POP,
                  REPRO_P2 = REPRO,
                  HABLOS_P3 = HABLOS,
                  PRED_P4 = PRED,
                  DRY_P5 = DRY,
                  VEG_P6 = VEG,
                  CHYTRI_P7 = CHYTRI,
                  ZASTIN_P8 = ZASTIN)
  
  result <- species_target_site 
  
  result
  
}

amp_bomvar_sci_eval <- function(species_name) {
  
  species_target_sci <- amp_bomvar_site_eval(species_name) %>%
    dplyr::group_by(EVL_SITECODE) %>%
    dplyr::summarise(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) != 9 ~ as.character(unique(NAZEV)[1]),
                               nchar(unique(EVL_SITECODE)) == 9 ~ as.character(find_evl_CODE_TO_NAME(unique(EVL_SITECODE))),
                               TRUE ~ as.character("HOVNO")),
      DRUH = species_name,
      PRESENCE_P1a = mean(na.omit(PRESENCE_P1a)),
      POP_P1b = mean(na.omit(POP_P1b)),
      REPRO_P2 = mean(na.omit(REPRO_P2)),
      HABLOS_P3 = mean(na.omit(HABLOS_P3)),
      PRED_P4 = mean(na.omit(PRED_P4)),
      DRY_P5 = mean(na.omit(DRY_P5)),
      VEG_P6 = mean(na.omit(VEG_P6)),
      CHYTRI_P7 = 1,
      ZASTIN_P8 = mean(na.omit(ZASTIN_P8)),
      OVERALL = mean(na.omit(OVERALL)),
      SUFFICIENT_SITES = sum(SUFFICIENT, na.rm = TRUE),
      TOTAL_SITES = n(),
      SUFFICIENT_PERC = SUFFICIENT_SITES/TOTAL_SITES
    )
  
  result <- species_target_sci
  
  result
  
}

#|-Lissotriton montantodi ----
amp_lismon_site_eval <- function(species_name) {
  
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
                                          POP_CELKPOCETVSEEXEMPLARREL == "do 10" ~ 1,
                                        TRUE ~ 1
      ),
      HABLOS_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                     STA_STAVVODALITORAL < 10 ~ 0,
                                     STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                     STA_STAVVODALITORAL >= 10 ~ 1,
                                     TRUE ~ 1),
      PRED_find = dplyr::case_when(STA_KACHNALOV == "ano" ~ 0,
                                   STA_ZOOPLANKTON == "ne" ~ 0,
                                   STA_KACHNALOV == "ne" ~ 1,
                                   STA_ZOOPLANKTON == "ano" ~ 1,
                                   TRUE ~ 1),
      WATER_find = dplyr::case_when(grepl("regulování vodní hladiny",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("chov ryb technologie",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("zaplavování pozdní",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("změny hydrografických poměrů obecně",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    TRUE ~ 1),
      DRY_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                  STA_STAVVODALITORAL < 10 ~ 0,
                                  STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                  STA_STAVVODALITORAL >= 10 ~ 1,
                                  grepl("vysychání (mokřadního ekosystému)",
                                        VLV_CINNOSTDOPAD) ~ 0,
                                  TRUE ~ 1),
      VEG_find = dplyr::case_when(STA_POKRVEGETACE >= 25 & 
                                    STA_POKRVEGETACE <= 75 ~ 1,
                                  STA_POKRVEGETACE < 25 ~ 0,
                                  STA_POKRVEGETACE > 75 ~ 0,
                                  TRUE ~ 1),
      TRANSP_find = dplyr::case_when(grepl("dno", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     grepl("", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     is.na(STA_PRUHLEDNOSTVODAPOZN) == TRUE ~ 1,
                                     STA_PRUHLEDNOSTVODA >= 50 ~ 1,
                                     STA_PRUHLEDNOSTVODA < 50 ~ 0,
                                     TRUE ~ 1),
      CHYTRI_find = 1,
      ZASTIN_find = dplyr::case_when(STA_ZASTINENI > 50 ~ 0,
                                     STA_ZASTINENI <= 50 ~ 1),
      THREATS_NUM_find = stringr::str_count(VLV_CINNOSTDOPAD, ","),
      TARGET_MON_find = 1,
      PRESENCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ano" ~ 1,
                                       POP_VYSKYTDRUH.1 == "ano" ~ 1,
                                       POP_DRUHANONE == "ano" ~ 1,
                                       ABUNDANCE_find > 0 ~ 1,
                                       POP_VYSKYTDRUH == "ne" ~ 0,
                                       POP_VYSKYTDRUH.1 == "ne" ~ 0,
                                       POP_DRUHANONE == "ne" ~ 0)
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
              POP = dplyr::case_when(PRESENCE_0 == 0 ~ 0,
                                     POP_0 >= POP_1 ~ 1,
                                     POP_0 < POP_1 ~ 0,
                                     POP_0 >= POP_2 ~ 1,
                                     POP_0 < POP_2 ~ 0,
                                     POP_1 >= POP_2 ~ 1,
                                     POP_1 < POP_2 ~ 0,
                                     length(unique(ROK)) < 2 ~ NA_real_),
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
              CHYTRI = 1,
              ZASTIN_0 = max(ZASTIN_find[ROK == current_year]),
              ZASTIN_1 = max(ZASTIN_find[ROK == current_year - 1]),
              ZASTIN_2 = max(ZASTIN_find[ROK == current_year - 2]),
              ZASTIN = dplyr::case_when(ZASTIN_0 != -Inf ~ as.numeric(ZASTIN_0),
                                        ZASTIN_1 != -Inf ~ as.numeric(ZASTIN_1),
                                        ZASTIN_2 != -Inf ~ as.numeric(ZASTIN_2)),
              NAZEV_LOKAL = unique(NAZ_LOKAL)[1]) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(
      OVERALL_sum = sum(PRESENCE,
                        POP,
                        REPRO,
                        HABLOS,
                        PRED,
                        WATER,
                        DRY,
                        CHYTRI,
                        na.rm = TRUE)
    )%>%
    dplyr::mutate(
      OVERALL = dplyr::case_when(OVERALL_sum == 8 ~ 1,
                                 OVERALL_sum < 8 &
                                   OVERALL_sum >=5 &
                                   PRESENCE == 1 ~ 0.5,
                                 OVERALL_sum < 5 |
                                   PRESENCE == 0 ~ 0),
      SUFFICIENT = dplyr::case_when(OVERALL == 1 ~ 1,
                                    OVERALL < 1 ~ 0)
    ) %>%
    dplyr::select(EVL_SITECODE,
                  SITECODE,
                  NAZEV,
                  DRUH,
                  PRESENCE,
                  POP,
                  REPRO,
                  HABLOS,
                  PRED,
                  WATER,
                  DRY,
                  CHYTRI,
                  OVERALL,
                  SUFFICIENT) %>%
    dplyr::rename(PRESENCE_P1a = PRESENCE,
                  POP_P1b = POP,
                  REPRO_P2 = REPRO,
                  HABLOS_P3 = HABLOS,
                  PRED_P4 = PRED,
                  WATER_P5 = WATER,
                  DRY_P6 = DRY,
                  CHYTRI_P7 = CHYTRI)
  
  result <- species_target_site 
  
  result
  
}

amp_lismon_sci_eval <- function(species_name) {
  
  species_target_sci <- amp_lismon_site_eval(species_name) %>%
    dplyr::group_by(EVL_SITECODE) %>%
    dplyr::summarise(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) != 9 ~ as.character(unique(NAZEV)[1]),
                               nchar(unique(EVL_SITECODE)) == 9 ~ as.character(find_evl_CODE_TO_NAME(unique(EVL_SITECODE))),
                               TRUE ~ as.character("HOVNO")),
      DRUH = species_name,
      PRESENCE_P1a = mean(na.omit(PRESENCE_P1a)),
      POP_P1b = mean(na.omit(POP_P1b)),
      REPRO_P2 = mean(na.omit(REPRO_P2)),
      HABLOS_P3 = mean(na.omit(HABLOS_P3)),
      PRED_P4 = mean(na.omit(PRED_P4)),
      WATER_P5 = mean(na.omit(WATER_P5)),
      DRY_P6 = mean(na.omit(DRY_P6)),
      CHYTRI_P7 = 1,
      OVERALL = mean(na.omit(OVERALL)),
      SUFFICIENT_SITES = sum(SUFFICIENT, na.rm = TRUE),
      TOTAL_SITES = n(),
      SUFFICIENT_PERC = SUFFICIENT_SITES/TOTAL_SITES
    )
  
  result <- species_target_sci
  
  result
  
}

#|-Triturus sp. ----
amp_tri_site_eval <- function(species_name) {
  
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
                                          POP_CELKPOCETVSEEXEMPLARREL == "do 10" ~ 1,
                                        TRUE ~ 1
      ),
      HABLOS_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                     STA_STAVVODALITORAL < 10 ~ 0,
                                     STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                     STA_STAVVODALITORAL >= 10 ~ 1,
                                     TRUE ~ 1),
      PRED_find = dplyr::case_when(STA_KACHNALOV == "ano" ~ 0,
                                   STA_ZOOPLANKTON == "ne" ~ 0,
                                   STA_KACHNALOV == "ne" ~ 1,
                                   STA_ZOOPLANKTON == "ano" ~ 1,
                                   TRUE ~ 1),
      WATER_find = dplyr::case_when(grepl("regulování vodní hladiny",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("chov ryb technologie",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("zaplavování pozdní",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    grepl("změny hydrografických poměrů obecně",
                                          VLV_CINNOSTDOPAD) ~ 0,
                                    TRUE ~ 1),
      DRY_find = dplyr::case_when(STA_STAVVODAPERIODTUN < 10 ~ 0,
                                  STA_STAVVODALITORAL < 10 ~ 0,
                                  STA_STAVVODAPERIODTUN >= 10 ~ 1,
                                  STA_STAVVODALITORAL >= 10 ~ 1,
                                  grepl("vysychání (mokřadního ekosystému)",
                                        VLV_CINNOSTDOPAD) ~ 0,
                                  TRUE ~ 1),
      VEG_find = dplyr::case_when(STA_POKRVEGETACE >= 25 & 
                                    STA_POKRVEGETACE <= 75 ~ 1,
                                  STA_POKRVEGETACE < 25 ~ 0,
                                  STA_POKRVEGETACE > 75 ~ 0),
      TRANSP_find = dplyr::case_when(grepl("dno", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     grepl("", STA_PRUHLEDNOSTVODAPOZN) ~ 1,
                                     is.na(STA_PRUHLEDNOSTVODAPOZN) == TRUE ~ 1,
                                     STA_PRUHLEDNOSTVODA >= 50 ~ 1,
                                     STA_PRUHLEDNOSTVODA < 50 ~ 0,
                                     TRUE ~ 1),
      BATRA_find = 1,
      ZASTIN_find = dplyr::case_when(STA_ZASTINENI > 50 ~ 0,
                                     STA_ZASTINENI <= 50 ~ 1),
      THREATS_NUM_find = stringr::str_count(VLV_CINNOSTDOPAD, ","),
      TARGET_MON_find = 1,
      PRESENCE_find = dplyr::case_when(POP_VYSKYTDRUH == "ano" ~ 1,
                                       POP_VYSKYTDRUH.1 == "ano" ~ 1,
                                       POP_DRUHANONE == "ano" ~ 1,
                                       ABUNDANCE_find > 0 ~ 1,
                                       POP_VYSKYTDRUH == "ne" ~ 0,
                                       POP_VYSKYTDRUH.1 == "ne" ~ 0,
                                       POP_DRUHANONE == "ne" ~ 0)
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
              POP = dplyr::case_when(PRESENCE_0 == 0 ~ 0,
                                     POP_0 >= POP_1 ~ 1,
                                     POP_0 < POP_1 ~ 0,
                                     POP_0 >= POP_2 ~ 1,
                                     POP_0 < POP_2 ~ 0,
                                     POP_1 >= POP_2 ~ 1,
                                     POP_1 < POP_2 ~ 0,
                                     length(unique(ROK)) < 2 ~ NA_real_),
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
              CHYTRI = 1,
              ZASTIN_0 = max(ZASTIN_find[ROK == current_year]),
              ZASTIN_1 = max(ZASTIN_find[ROK == current_year - 1]),
              ZASTIN_2 = max(ZASTIN_find[ROK == current_year - 2]),
              ZASTIN = dplyr::case_when(ZASTIN_0 != -Inf ~ as.numeric(ZASTIN_0),
                                        ZASTIN_1 != -Inf ~ as.numeric(ZASTIN_1),
                                        ZASTIN_2 != -Inf ~ as.numeric(ZASTIN_2)),
              NAZEV_LOKAL = unique(NAZ_LOKAL)[1]) %>%
    dplyr::group_by(SITECODE) %>%
    dplyr::mutate(
      OVERALL_sum = sum(PRESENCE,
                        POP,
                        REPRO,
                        HABLOS,
                        PRED,
                        WATER,
                        DRY,
                        VEG,
                        TRANSP,
                        CHYTRI,
                        na.rm = TRUE)
    )%>%
    dplyr::mutate(
      OVERALL = dplyr::case_when(OVERALL_sum == 10 ~ 1,
                                 OVERALL_sum < 10 &
                                   OVERALL_sum >= 7 &
                                   PRESENCE == 1 ~ 0.5,
                                 OVERALL_sum < 7 |
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
                  REPRO,
                  HABLOS,
                  PRED,
                  WATER,
                  DRY,
                  VEG,
                  TRANSP,
                  CHYTRI,
                  OVERALL,
                  SUFFICIENT) %>%
    dplyr::rename(PRESENCE_P1a = PRESENCE,
                  POP_P1b = POP,
                  REPRO_P2 = REPRO,
                  HABLOS_P3 = HABLOS,
                  PRED_P4 = PRED,
                  WATER_P5 = WATER,
                  DRY_P6 = DRY,
                  VEG_P7 = VEG,
                  TRANSP_P8 = TRANSP,
                  CHYTRI_P9 = CHYTRI)
  
  result <- species_target_site 
  
  result
  
}

amp_tri_sci_eval <- function(species_name) {
  
  species_target_sci <- amp_tri_site_eval(species_name) %>%
    dplyr::group_by(EVL_SITECODE) %>%
    dplyr::summarise(
      NAZEV = dplyr::case_when(nchar(unique(EVL_SITECODE)) != 9 ~ as.character(unique(NAZEV)[1]),
                               nchar(unique(EVL_SITECODE)) == 9 ~ as.character(find_evl_CODE_TO_NAME(unique(EVL_SITECODE))),
                               TRUE ~ as.character("HOVNO")),
      DRUH = species_name,
      PRESENCE_P1a = mean(na.omit(PRESENCE_P1a)),
      POP_P1b = mean(na.omit(POP_P1b)),
      REPRO_P2 = mean(na.omit(REPRO_P2)),
      HABLOS_P3 = mean(na.omit(HABLOS_P3)),
      PRED_P4 = mean(na.omit(PRED_P4)),
      WATER_P5 = mean(na.omit(WATER_P5)),
      DRY_P6 = mean(na.omit(DRY_P6)),
      VEG_P7 = mean(na.omit(VEG_P7)),
      TRANSP_P8 = mean(na.omit(TRANSP_P8)),
      CHYTRI_P9 = mean(na.omit(CHYTRI_P9)),
      # CELKOVE HODNOCENI LOKALITY
      OVERALL = mean(na.omit(OVERALL)),
      # POCET LOKALIT V DOSTATECNEM STAVU
      SUFFICIENT_SITES = sum(SUFFICIENT, na.rm = TRUE),
      # CELKOVY POCET LOKALIT
      TOTAL_SITES = n(),
      SUFFICIENT_PERC = SUFFICIENT_SITES/TOTAL_SITES
    )
  
  result <- species_target_sci
  
  result
  
}

# RESULTS ----
bombom_site_eval <- amp_bombom_site_eval("Bombina bombina")
bomvar_site_eval <- amp_bomvar_site_eval("Bombina variegata")
tricri_site_eval <- amp_tri_site_eval("Triturus cristatus")
tricar_site_eval <- amp_tri_site_eval("Triturus carnifex")
lismon_site_eval <- amp_lismon_site_eval("Lissotriton montandoni")

bombom_sci_eval <- amp_bombom_sci_eval("Bombina bombina")
bomvar_sci_eval <- amp_bomvar_sci_eval("Bombina variegata")
tricri_sci_eval <- amp_tri_sci_eval("Triturus cristatus")
tricar_sci_eval <- amp_tri_sci_eval("Triturus carnifex")
lismon_sci_eval <- amp_lismon_sci_eval("Lissotriton montandoni")


# Bombina bombina SITES
write.csv(bombom_site_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_bombom_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(bombom_site_eval, 
          "C:/Users/jonas.gaigr/Desktop/results_bombom_sites.csv",
          row.names = FALSE)
# Bombia variegata SITES
write.csv(bomvar_site_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_bomvar_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(bomvar_site_eval, 
          "C:/Users/jonas.gaigr/Desktop/results_bomvar_sites.csv",
          row.names = FALSE)
# Lissotriton montandoni SITES
write.csv(lismon_site_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_lismon_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(lismon_site_eval, 
          "C:/Users/jonas.gaigr/Desktop/results_lismon_sites.csv",
          row.names = FALSE)
# Triturus sp. SITES
write.csv(tricri_site_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_tricri_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(tricri_site_eval, 
          "C:/Users/jonas.gaigr/Desktop/results_tricri_sites.csv",
          row.names = FALSE)
write.csv(tricar_site_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_tricar_sites.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(tricar_site_eval, 
          "C:/Users/jonas.gaigr/Desktop/results_tricar_sites.csv",
          row.names = FALSE)


# Bombina bombina SCI
write.csv(bombom_sci_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_bombom_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(bombom_sci_eval, 
           "C:/Users/jonas.gaigr/Desktop/results_bombom_sci.csv",
           row.names = FALSE)
# Bombia variegata SCI
write.csv(bomvar_sci_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_bomvar_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(bomvar_sci_eval, 
           "C:/Users/jonas.gaigr/Desktop/results_bomvar_sci.csv",
           row.names = FALSE)
# Lissotriton montandoni SCI
write.csv(lismon_sci_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_lismon_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(lismon_sci_eval, 
           "C:/Users/jonas.gaigr/Desktop/results_lismon_sci.csv",
           row.names = FALSE)
# Triturus sp. SCI
write.csv(tricri_sci_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_tricri_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(tricri_sci_eval, 
           "C:/Users/jonas.gaigr/Desktop/results_tricri_sci.csv",
           row.names = FALSE)
write.csv(tricar_sci_eval, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/results_tricar_sci.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(tricar_sci_eval, 
           "C:/Users/jonas.gaigr/Desktop/results_tricar_sci.csv",
           row.names = FALSE)

bombom_sci_eval %>% filter(is.na(POP_P1b) == FALSE)
# DATATABLE ----
library(DT)
# |-Bombina bombina ----
amp_bombom_site_dt <- DT::datatable(bombom_site_eval %>%
                                      dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                      POP_P1b,
                                                                                      REPRO_P2,
                                                                                      HABLOS_P3,
                                                                                      PRED_P4,
                                                                                      WATER_P5,
                                                                                      DRY_P6,
                                                                                      VEG_P7,
                                                                                      TRANSP_P8,
                                                                                      CHYTRI_P9,
                                                                                      ZASTIN_P10)) == TRUE ~ 0,
                                                                            TRUE ~ 1),
                                                    ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                               EVAL == 0 ~ -1,
                                                                               OVERALL == 1 ~ 1,
                                                                               OVERALL < 1 & 
                                                                                 PRESENCE_P1a == 1 ~ 0.5),
                                                    PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                    PRESENCE_P1a == 0 ~ 0,
                                                                                    is.na(PRESENCE_P1a) == TRUE ~ -1),
                                                    POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                               POP_P1b < 0.5~ 0,
                                                                               is.na(POP_P1b) == TRUE ~ -1),
                                                    REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                 REPRO_P2 < 0.5  ~ 0,
                                                                                 is.na(REPRO_P2) == TRUE ~ -1),
                                                    HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                  HABLOS_P3 < 0.5 ~ 0,
                                                                                  is.na(HABLOS_P3) == TRUE ~ -1),
                                                    PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                                PRED_P4 < 0.5 ~ 0,
                                                                                is.na(PRED_P4) == TRUE ~ -1),
                                                    WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                 WATER_P5 < 0.5 ~ 0,
                                                                                 is.na(WATER_P5) == TRUE ~ -1),
                                                    DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                               DRY_P6 < 0.5 ~ 0,
                                                                               is.na(DRY_P6) == TRUE ~ -1),
                                                    VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                               VEG_P7 < 0.5 ~ 0,
                                                                               is.na(VEG_P7) == TRUE ~ -1),
                                                    TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                  TRANSP_P8 < 0.5 ~ 0,
                                                                                  is.na(TRANSP_P8) == TRUE ~ -1),
                                                    CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                  CHYTRI_P9 < 0.5 ~ 0,
                                                                                  is.na(CHYTRI_P9) == TRUE ~ -1),
                                                    ZASTIN_COL = dplyr::case_when(ZASTIN_P10 >= 0.5 ~ 1,
                                                                                  ZASTIN_P10 < 0.5 ~ 0,
                                                                                  is.na(ZASTIN_P10) == TRUE ~ -1)) %>%
                                      dplyr::mutate(across(c(PRESENCE_P1a,
                                                             POP_P1b,
                                                             REPRO_P2,
                                                             HABLOS_P3,
                                                             PRED_P4,
                                                             WATER_P5,
                                                             DRY_P6,
                                                             VEG_P7,
                                                             TRANSP_P8,
                                                             CHYTRI_P9,
                                                             ZASTIN_P10,
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
                                      columnDefs = list(list(targets = c(18:29), visible = FALSE))),
                                    rownames = FALSE,
                                    filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ZASTIN_P10', 'ZASTIN_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

amp_bombom_sci_dt <- DT::datatable(bombom_sci_eval %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                     POP_P1b,
                                                                                     REPRO_P2,
                                                                                     HABLOS_P3,
                                                                                     PRED_P4,
                                                                                     WATER_P5,
                                                                                     DRY_P6,
                                                                                     VEG_P7,
                                                                                     TRANSP_P8,
                                                                                     CHYTRI_P9,
                                                                                     ZASTIN_P10)) == TRUE ~ 0,
                                                                           TRUE ~ 1),
                                                   ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5,
                                                                              PRESENCE_P1a == 0 ~ 0,
                                                                              is.na(PRESENCE_P1a) == TRUE ~ -1),
                                                   POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                              POP_P1b < 0.5~ 0,
                                                                              is.na(POP_P1b) == TRUE ~ -1),
                                                   REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                REPRO_P2 < 0.5  ~ 0,
                                                                                is.na(REPRO_P2) == TRUE ~ -1),
                                                   HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                 HABLOS_P3 < 0.5 ~ 0,
                                                                                 is.na(HABLOS_P3) == TRUE ~ -1),
                                                   PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                               PRED_P4 < 0.5 ~ 0,
                                                                               is.na(PRED_P4) == TRUE ~ -1),
                                                   WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                WATER_P5 < 0.5 ~ 0,
                                                                                is.na(WATER_P5) == TRUE ~ -1),
                                                   DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                              DRY_P6 < 0.5 ~ 0,
                                                                              is.na(DRY_P6) == TRUE ~ -1),
                                                   VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                              VEG_P7 < 0.5 ~ 0,
                                                                              is.na(VEG_P7) == TRUE ~ -1),
                                                   TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                 TRANSP_P8 < 0.5 ~ 0,
                                                                                 is.na(TRANSP_P8) == TRUE ~ -1),
                                                   CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                 CHYTRI_P9 < 0.5 ~ 0,
                                                                                 is.na(CHYTRI_P9) == TRUE ~ -1),
                                                   ZASTIN_COL = dplyr::case_when(ZASTIN_P10 >= 0.5 ~ 1,
                                                                                 ZASTIN_P10 < 0.5 ~ 0,
                                                                                 is.na(ZASTIN_P10) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_P1a,
                                                            POP_P1b,
                                                            REPRO_P2,
                                                            HABLOS_P3,
                                                            PRED_P4,
                                                            WATER_P5,
                                                            DRY_P6,
                                                            VEG_P7,
                                                            TRANSP_P8,
                                                            CHYTRI_P9,
                                                            ZASTIN_P10,
                                                            OVERALL,
                                                            SUFFICIENT_PERC),
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
                                     columnDefs = list(list(targets = c(19:30), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                                  c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ZASTIN_P10', 'ZASTIN_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT_SITES', 'SUFFICIENT_PERC', 
                  backgroundColor = styleInterval(0.5, 
                                                  c("red", "green")))
amp_bombom_site_dt
amp_bombom_sci_dt

# |-Bombina variegata ----
amp_bomvar_site_dt <- DT::datatable(bomvar_site_eval %>%
                                      dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                      POP_P1b,
                                                                                      REPRO_P2,
                                                                                      HABLOS_P3,
                                                                                      PRED_P4,
                                                                                      DRY_P5,
                                                                                      VEG_P6,
                                                                                      CHYTRI_P7,
                                                                                      ZASTIN_P8)) == TRUE ~ 0,
                                                                            TRUE ~ 1),
                                                    ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                               EVAL == 0 ~ -1,
                                                                               OVERALL == 1 ~ 1,
                                                                               OVERALL < 1 & 
                                                                                 PRESENCE_P1a == 1 ~ 0.5),
                                                    PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                    OVERALL < 0.5 & 
                                                                                      PRESENCE_P1a > 0 ~ 0.5,
                                                                                    PRESENCE_P1a == 0 ~ 0),
                                                    POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                               POP_P1b < 0.5~ 0,
                                                                               is.na(POP_P1b) == TRUE ~ -1),
                                                    REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                 REPRO_P2 < 0.5  ~ 0,
                                                                                 is.na(REPRO_P2) == TRUE ~ -1),
                                                    HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                  HABLOS_P3 < 0.5 ~ 0,
                                                                                  is.na(HABLOS_P3) == TRUE ~ -1),
                                                    PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                                PRED_P4 < 0.5 ~ 0,
                                                                                is.na(PRED_P4) == TRUE ~ -1),
                                                    DRY_COL = dplyr::case_when(DRY_P5 >= 0.5 ~ 1,
                                                                               DRY_P5 < 0.5 ~ 0,
                                                                               is.na(DRY_P5) == TRUE ~ -1),
                                                    VEG_COL = dplyr::case_when(VEG_P6 >= 0.5 ~ 1,
                                                                               VEG_P6 < 0.5 ~ 0,
                                                                               is.na(VEG_P6) == TRUE ~ -1),
                                                    CHYTRI_COL = dplyr::case_when(CHYTRI_P7 >= 0.5 ~ 1,
                                                                                  CHYTRI_P7 < 0.5 ~ 0,
                                                                                  is.na(CHYTRI_P7) == TRUE ~ -1),
                                                    ZASTIN_COL = dplyr::case_when(ZASTIN_P8 >= 0.5 ~ 1,
                                                                                  ZASTIN_P8 < 0.5 ~ 0,
                                                                                  is.na(ZASTIN_P8) == TRUE ~ -1)) %>%
                                      dplyr::mutate(across(c(PRESENCE_P1a,
                                                             POP_P1b,
                                                             REPRO_P2,
                                                             HABLOS_P3,
                                                             PRED_P4,
                                                             DRY_P5,
                                                             VEG_P6,
                                                             CHYTRI_P7,
                                                             ZASTIN_P8,
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
                                      columnDefs = list(list(targets = c(16:25), visible = FALSE))),
                                    rownames = FALSE,
                                    filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P5', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P6', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P7', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ZASTIN_P8', 'ZASTIN_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

amp_bomvar_sci_dt <- DT::datatable(bomvar_sci_eval %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                     POP_P1b,
                                                                                     REPRO_P2,
                                                                                     HABLOS_P3,
                                                                                     PRED_P4,
                                                                                     DRY_P5,
                                                                                     VEG_P6,
                                                                                     CHYTRI_P7,
                                                                                     ZASTIN_P8)) == TRUE ~ 0,
                                                                           TRUE ~ 1),
                                                   ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                   SUFFICIENT_PERC < 0.5 & 
                                                                                     PRESENCE_P1a > 0 ~ 0.5,
                                                                                   PRESENCE_P1a == 0 ~ 0),
                                                   POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                              POP_P1b < 0.5~ 0,
                                                                              is.na(POP_P1b) == TRUE ~ -1),
                                                   REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                REPRO_P2 < 0.5  ~ 0,
                                                                                is.na(REPRO_P2) == TRUE ~ -1),
                                                   HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                 HABLOS_P3 < 0.5 ~ 0,
                                                                                 is.na(HABLOS_P3) == TRUE ~ -1),
                                                   PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                               PRED_P4 < 0.5 ~ 0,
                                                                               is.na(PRED_P4) == TRUE ~ -1),
                                                   DRY_COL = dplyr::case_when(DRY_P5 >= 0.5 ~ 1,
                                                                              DRY_P5 < 0.5 ~ 0,
                                                                              is.na(DRY_P5) == TRUE ~ -1),
                                                   VEG_COL = dplyr::case_when(VEG_P6 >= 0.5 ~ 1,
                                                                              VEG_P6 < 0.5 ~ 0,
                                                                              is.na(VEG_P6) == TRUE ~ -1),
                                                   CHYTRI_COL = dplyr::case_when(CHYTRI_P7 >= 0.5 ~ 1,
                                                                                 CHYTRI_P7 < 0.5 ~ 0,
                                                                                 is.na(CHYTRI_P7) == TRUE ~ -1),
                                                   ZASTIN_COL = dplyr::case_when(ZASTIN_P8 >= 0.5 ~ 1,
                                                                                 ZASTIN_P8 < 0.5 ~ 0,
                                                                                 is.na(ZASTIN_P8) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_P1a,
                                                            POP_P1b,
                                                            REPRO_P2,
                                                            HABLOS_P3,
                                                            PRED_P4,
                                                            DRY_P5,
                                                            VEG_P6,
                                                            CHYTRI_P7,
                                                            ZASTIN_P8,
                                                            OVERALL,
                                                            SUFFICIENT_PERC),
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
                                     columnDefs = list(list(targets = c(17:26), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P5', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P6', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P7', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('ZASTIN_P8', 'ZASTIN_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT_SITES', 'SUFFICIENT_PERC', 
                  backgroundColor = styleInterval(0.5, 
                                                  c("red", "green")))

amp_bomvar_site_dt
amp_bomvar_sci_dt
  
# |-Lissotriton montandoni ----
amp_lismon_site_dt <- DT::datatable(lismon_site_eval %>%
                                      dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                      POP_P1b,
                                                                                      REPRO_P2,
                                                                                      HABLOS_P3,
                                                                                      PRED_P4,
                                                                                      WATER_P5,
                                                                                      DRY_P6,
                                                                                      CHYTRI_P7)) == TRUE ~ 0,
                                                                            TRUE ~ 1),
                                                    ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                               EVAL == 0 ~ -1,
                                                                               OVERALL == 1 ~ 1,
                                                                               OVERALL < 1 & 
                                                                                 PRESENCE_P1a == 1 ~ 0.5),
                                                    PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                    OVERALL < 0.5 & 
                                                                                      PRESENCE_P1a > 0 ~ 0.5,
                                                                                    PRESENCE_P1a == 0 ~ 0),
                                                    POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                               POP_P1b < 0.5~ 0,
                                                                               is.na(POP_P1b) == TRUE ~ -1),
                                                    REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                 REPRO_P2 < 0.5  ~ 0,
                                                                                 is.na(REPRO_P2) == TRUE ~ -1),
                                                    HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                  HABLOS_P3 < 0.5 ~ 0,
                                                                                  is.na(HABLOS_P3) == TRUE ~ -1),
                                                    PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                                PRED_P4 < 0.5 ~ 0,
                                                                                is.na(PRED_P4) == TRUE ~ -1),
                                                    WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                               WATER_P5 < 0.5 ~ 0,
                                                                               is.na(WATER_P5) == TRUE ~ -1),
                                                    DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                               DRY_P6 < 0.5 ~ 0,
                                                                               is.na(DRY_P6) == TRUE ~ -1),
                                                    CHYTRI_COL = dplyr::case_when(CHYTRI_P7 >= 0.5 ~ 1,
                                                                                  CHYTRI_P7 < 0.5 ~ 0,
                                                                                  is.na(CHYTRI_P7) == TRUE ~ -1)) %>%
                                      dplyr::mutate(across(c(PRESENCE_P1a,
                                                             POP_P1b,
                                                             REPRO_P2,
                                                             HABLOS_P3,
                                                             PRED_P4,
                                                             WATER_P5,
                                                             DRY_P6,
                                                             CHYTRI_P7,
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
                                      columnDefs = list(list(targets = c(15:23), visible = FALSE))),
                                    rownames = FALSE,
                                    filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P7', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

amp_lismon_sci_dt <- DT::datatable(lismon_sci_eval %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                     POP_P1b,
                                                                                     REPRO_P2,
                                                                                     HABLOS_P3,
                                                                                     PRED_P4,
                                                                                     WATER_P5,
                                                                                     DRY_P6,
                                                                                     CHYTRI_P7)) == TRUE ~ 0,
                                                                           TRUE ~ 1),
                                                   ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                   SUFFICIENT_PERC < 0.5 & 
                                                                                     PRESENCE_P1a > 0 ~ 0.5,
                                                                                   PRESENCE_P1a == 0 ~ 0),
                                                   POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                              POP_P1b < 0.5~ 0,
                                                                              is.na(POP_P1b) == TRUE ~ -1),
                                                   REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                REPRO_P2 < 0.5  ~ 0,
                                                                                is.na(REPRO_P2) == TRUE ~ -1),
                                                   HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                 HABLOS_P3 < 0.5 ~ 0,
                                                                                 is.na(HABLOS_P3) == TRUE ~ -1),
                                                   PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                               PRED_P4 < 0.5 ~ 0,
                                                                               is.na(PRED_P4) == TRUE ~ -1),
                                                   WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                WATER_P5 < 0.5 ~ 0,
                                                                                is.na(WATER_P5) == TRUE ~ -1),
                                                   DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                              DRY_P6 < 0.5 ~ 0,
                                                                              is.na(DRY_P6) == TRUE ~ -1),
                                                   CHYTRI_COL = dplyr::case_when(CHYTRI_P7 >= 0.5 ~ 1,
                                                                                 CHYTRI_P7 < 0.5 ~ 0,
                                                                                 is.na(CHYTRI_P7) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_P1a,
                                                            POP_P1b,
                                                            REPRO_P2,
                                                            HABLOS_P3,
                                                            PRED_P4,
                                                            WATER_P5,
                                                            DRY_P6,
                                                            CHYTRI_P7,
                                                            OVERALL,
                                                            SUFFICIENT_PERC),
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
                                     columnDefs = list(list(targets = c(17:26), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P7', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))  %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT_SITES', 'SUFFICIENT_PERC', 
                  backgroundColor = styleInterval(0.5, 
                                                  c("red", "green")))

amp_lismon_site_dt
amp_lismon_sci_dt
# |-Triturus cristatus ----
amp_tricri_site_dt <- DT::datatable(tricri_site_eval %>%
                                      dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                      POP_P1b,
                                                                                      REPRO_P2,
                                                                                      HABLOS_P3,
                                                                                      PRED_P4,
                                                                                      WATER_P5,
                                                                                      DRY_P6,
                                                                                      VEG_P7,
                                                                                      TRANSP_P8,
                                                                                      CHYTRI_P9)) == TRUE ~ 0,
                                                                            TRUE ~ 1),
                                                    ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                               EVAL == 0 ~ -1,
                                                                               OVERALL == 1 ~ 1,
                                                                               OVERALL < 1 & 
                                                                                 PRESENCE_P1a == 1 ~ 0.5),
                                                    PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                    OVERALL < 0.5 & 
                                                                                      PRESENCE_P1a > 0 ~ 0.5,
                                                                                    PRESENCE_P1a == 0 ~ 0),
                                                    POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                               POP_P1b < 0.5~ 0,
                                                                               is.na(POP_P1b) == TRUE ~ -1),
                                                    REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                 REPRO_P2 < 0.5  ~ 0,
                                                                                 is.na(REPRO_P2) == TRUE ~ -1),
                                                    HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                  HABLOS_P3 < 0.5 ~ 0,
                                                                                  is.na(HABLOS_P3) == TRUE ~ -1),
                                                    PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                                PRED_P4 < 0.5 ~ 0,
                                                                                is.na(PRED_P4) == TRUE ~ -1),
                                                    WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                 WATER_P5 < 0.5 ~ 0,
                                                                                 is.na(WATER_P5) == TRUE ~ -1),
                                                    DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                               DRY_P6 < 0.5 ~ 0,
                                                                               is.na(DRY_P6) == TRUE ~ -1),
                                                    VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                               VEG_P7 < 0.5 ~ 0,
                                                                               is.na(VEG_P7) == TRUE ~ -1),
                                                    TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                  TRANSP_P8 < 0.5 ~ 0,
                                                                                  is.na(TRANSP_P8) == TRUE ~ -1),
                                                    CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                  CHYTRI_P9 < 0.5 ~ 0,
                                                                                  is.na(CHYTRI_P9) == TRUE ~ -1)) %>%
                                      dplyr::mutate(across(c(PRESENCE_P1a,
                                                             POP_P1b,
                                                             REPRO_P2,
                                                             HABLOS_P3,
                                                             PRED_P4,
                                                             WATER_P5,
                                                             DRY_P6,
                                                             VEG_P7,
                                                             TRANSP_P8,
                                                             CHYTRI_P9,
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
                                      columnDefs = list(list(targets = c(17:27), visible = FALSE))),
                                    rownames = FALSE,
                                    filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

amp_tricri_sci_dt <- DT::datatable(tricri_sci_eval %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                     POP_P1b,
                                                                                     REPRO_P2,
                                                                                     HABLOS_P3,
                                                                                     PRED_P4,
                                                                                     WATER_P5,
                                                                                     DRY_P6,
                                                                                     VEG_P7,
                                                                                     TRANSP_P8,
                                                                                     CHYTRI_P9)) == TRUE ~ 0,
                                                                           TRUE ~ 1),
                                                   ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                   SUFFICIENT_PERC < 0.5 & 
                                                                                     PRESENCE_P1a > 0 ~ 0.5,
                                                                                   PRESENCE_P1a == 0 ~ 0),
                                                   POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                              POP_P1b < 0.5~ 0,
                                                                              is.na(POP_P1b) == TRUE ~ -1),
                                                   REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                REPRO_P2 < 0.5  ~ 0,
                                                                                is.na(REPRO_P2) == TRUE ~ -1),
                                                   HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                 HABLOS_P3 < 0.5 ~ 0,
                                                                                 is.na(HABLOS_P3) == TRUE ~ -1),
                                                   PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                               PRED_P4 < 0.5 ~ 0,
                                                                               is.na(PRED_P4) == TRUE ~ -1),
                                                   WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                WATER_P5 < 0.5 ~ 0,
                                                                                is.na(WATER_P5) == TRUE ~ -1),
                                                   DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                              DRY_P6 < 0.5 ~ 0,
                                                                              is.na(DRY_P6) == TRUE ~ -1),
                                                   VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                              VEG_P7 < 0.5 ~ 0,
                                                                              is.na(VEG_P7) == TRUE ~ -1),
                                                   TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                 TRANSP_P8 < 0.5 ~ 0,
                                                                                 is.na(TRANSP_P8) == TRUE ~ -1),
                                                   CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                 CHYTRI_P9 < 0.5 ~ 0,
                                                                                 is.na(CHYTRI_P9) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_P1a,
                                                            POP_P1b,
                                                            REPRO_P2,
                                                            HABLOS_P3,
                                                            PRED_P4,
                                                            WATER_P5,
                                                            DRY_P6,
                                                            VEG_P7,
                                                            TRANSP_P8,
                                                            CHYTRI_P9,
                                                            OVERALL,
                                                            SUFFICIENT_PERC),
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
                                     columnDefs = list(list(targets = c(18:28), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT_SITES', 'SUFFICIENT_PERC', 
                  backgroundColor = styleInterval(0.5, 
                                                  c("red", "green")))

amp_tricri_site_dt
amp_tricri_sci_dt

# |-Triturus carnifex ----
amp_tricar_site_dt <- DT::datatable(tricar_site_eval %>%
                                      dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                      POP_P1b,
                                                                                      REPRO_P2,
                                                                                      HABLOS_P3,
                                                                                      PRED_P4,
                                                                                      WATER_P5,
                                                                                      DRY_P6,
                                                                                      VEG_P7,
                                                                                      TRANSP_P8,
                                                                                      CHYTRI_P9)) == TRUE ~ 0,
                                                                            TRUE ~ 1),
                                                    ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                               EVAL == 0 ~ -1,
                                                                               OVERALL == 1 ~ 1,
                                                                               OVERALL < 1 & 
                                                                                 PRESENCE_P1a == 1 ~ 0.5),
                                                    PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                    OVERALL < 0.5 & 
                                                                                      PRESENCE_P1a > 0 ~ 0.5,
                                                                                    PRESENCE_P1a == 0 ~ 0),
                                                    POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                               POP_P1b < 0.5~ 0,
                                                                               is.na(POP_P1b) == TRUE ~ -1),
                                                    REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                 REPRO_P2 < 0.5  ~ 0,
                                                                                 is.na(REPRO_P2) == TRUE ~ -1),
                                                    HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                  HABLOS_P3 < 0.5 ~ 0,
                                                                                  is.na(HABLOS_P3) == TRUE ~ -1),
                                                    PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                                PRED_P4 < 0.5 ~ 0,
                                                                                is.na(PRED_P4) == TRUE ~ -1),
                                                    WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                 WATER_P5 < 0.5 ~ 0,
                                                                                 is.na(WATER_P5) == TRUE ~ -1),
                                                    DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                               DRY_P6 < 0.5 ~ 0,
                                                                               is.na(DRY_P6) == TRUE ~ -1),
                                                    VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                               VEG_P7 < 0.5 ~ 0,
                                                                               is.na(VEG_P7) == TRUE ~ -1),
                                                    TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                  TRANSP_P8 < 0.5 ~ 0,
                                                                                  is.na(TRANSP_P8) == TRUE ~ -1),
                                                    CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                  CHYTRI_P9 < 0.5 ~ 0,
                                                                                  is.na(CHYTRI_P9) == TRUE ~ -1)) %>%
                                      dplyr::mutate(across(c(PRESENCE_P1a,
                                                             POP_P1b,
                                                             REPRO_P2,
                                                             HABLOS_P3,
                                                             PRED_P4,
                                                             WATER_P5,
                                                             DRY_P6,
                                                             VEG_P7,
                                                             TRANSP_P8,
                                                             CHYTRI_P9,
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
                                      columnDefs = list(list(targets = c(17:27), visible = FALSE))),
                                    rownames = FALSE,
                                    filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green")))

amp_tricar_sci_dt <- DT::datatable(tricar_sci_eval %>%
                                     dplyr::mutate(EVAL = dplyr::case_when(is.na(sum(PRESENCE_P1a,
                                                                                     POP_P1b,
                                                                                     REPRO_P2,
                                                                                     HABLOS_P3,
                                                                                     PRED_P4,
                                                                                     WATER_P5,
                                                                                     DRY_P6,
                                                                                     VEG_P7,
                                                                                     TRANSP_P8,
                                                                                     CHYTRI_P9)) == TRUE ~ 0,
                                                                           TRUE ~ 1),
                                                   ROW_COL = dplyr::case_when(PRESENCE_P1a == 0 ~ 0,
                                                                              EVAL == 0 ~ -1,
                                                                              SUFFICIENT_PERC >= 0.5 ~ 1,
                                                                              SUFFICIENT_PERC < 0.5 & 
                                                                                PRESENCE_P1a > 0 ~ 0.5),
                                                   PRESENCE_COL = dplyr::case_when(PRESENCE_P1a >= 0.5 ~ 1,
                                                                                   SUFFICIENT_PERC < 0.5 & 
                                                                                     PRESENCE_P1a > 0 ~ 0.5,
                                                                                   PRESENCE_P1a == 0 ~ 0),
                                                   POP_COL = dplyr::case_when(POP_P1b >= 0.5 ~ 1,
                                                                              POP_P1b < 0.5~ 0,
                                                                              is.na(POP_P1b) == TRUE ~ -1),
                                                   REPRO_COL = dplyr::case_when(REPRO_P2 >= 0.5 ~ 1,
                                                                                REPRO_P2 < 0.5  ~ 0,
                                                                                is.na(REPRO_P2) == TRUE ~ -1),
                                                   HABLOS_COL = dplyr::case_when(HABLOS_P3 >= 0.5 ~ 1,
                                                                                 HABLOS_P3 < 0.5 ~ 0,
                                                                                 is.na(HABLOS_P3) == TRUE ~ -1),
                                                   PRED_COL = dplyr::case_when(PRED_P4 >= 0.5 ~ 1,
                                                                               PRED_P4 < 0.5 ~ 0,
                                                                               is.na(PRED_P4) == TRUE ~ -1),
                                                   WATER_COL = dplyr::case_when(WATER_P5 >= 0.5 ~ 1,
                                                                                WATER_P5 < 0.5 ~ 0,
                                                                                is.na(WATER_P5) == TRUE ~ -1),
                                                   DRY_COL = dplyr::case_when(DRY_P6 >= 0.5 ~ 1,
                                                                              DRY_P6 < 0.5 ~ 0,
                                                                              is.na(DRY_P6) == TRUE ~ -1),
                                                   VEG_COL = dplyr::case_when(VEG_P7 >= 0.5 ~ 1,
                                                                              VEG_P7 < 0.5 ~ 0,
                                                                              is.na(VEG_P7) == TRUE ~ -1),
                                                   TRANSP_COL = dplyr::case_when(TRANSP_P8 >= 0.5 ~ 1,
                                                                                 TRANSP_P8 < 0.5 ~ 0,
                                                                                 is.na(TRANSP_P8) == TRUE ~ -1),
                                                   CHYTRI_COL = dplyr::case_when(CHYTRI_P9 >= 0.5 ~ 1,
                                                                                 CHYTRI_P9 < 0.5 ~ 0,
                                                                                 is.na(CHYTRI_P9) == TRUE ~ -1)) %>%
                                     dplyr::mutate(across(c(PRESENCE_P1a,
                                                            POP_P1b,
                                                            REPRO_P2,
                                                            HABLOS_P3,
                                                            PRED_P4,
                                                            WATER_P5,
                                                            DRY_P6,
                                                            VEG_P7,
                                                            TRANSP_P8,
                                                            CHYTRI_P9,
                                                            OVERALL,
                                                            SUFFICIENT_PERC),
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
                                     columnDefs = list(list(targets = c(18:28), visible = FALSE))),
                                   rownames = FALSE,
                                   filter = "top") %>%
  DT::formatStyle('EVL_SITECODE', 'ROW_COL', 
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SITECODE', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRUH', 'ROW_COL', 
                  backgroundColor =  styleEqual(c(-1, 0, 0.5, 1), 
                                                c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRESENCE_P1a', 'PRESENCE_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('POP_P1b', 'POP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('REPRO_P2', 'REPRO_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('HABLOS_P3', 'HABLOS_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('PRED_P4', 'PRED_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('WATER_P5', 'WATER_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('DRY_P6', 'DRY_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('VEG_P7', 'VEG_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('TRANSP_P8', 'TRANSP_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('CHYTRI_P9', 'CHYTRI_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('OVERALL', 'ROW_COL',
                  backgroundColor = styleEqual(c(-1, 0, 0.5, 1), 
                                               c("grey", "red", "orange", "green"))) %>%
  DT::formatStyle('SUFFICIENT_SITES', 'SUFFICIENT_PERC', 
                  backgroundColor = styleInterval(0.5, 
                                                  c("red", "green")))

amp_tricar_site_dt
amp_tricar_sci_dt
