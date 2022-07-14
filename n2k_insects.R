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
Lep.1.clear <- function(species) {
  species <- species %>%
    mutate(
      # Přítomnost druhu
      PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                          (is.na(POCET) == FALSE & POCET > 0) | 
                            (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
      # Převedení dostupných dat o početnosti na jednotné kategorie početnosti
      POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                              REL_POC == "ojediněle" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci" |
                                 POCITANO == "samice" | POCITANO == "samci") ~ 1,
                            (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                              REL_POC == "vzácně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci" |
                                 POCITANO == "samice" | POCITANO == "samci") ~ 2,
                            (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                              REL_POC == "roztroušeně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci" |
                                 POCITANO == "samice" | POCITANO == "samci") ~ 3,
                            (POCET > 100) | REL_POC == "hojně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci" |
                                 POCITANO == "samice" | POCITANO == "samci") ~ 4),
      # Vyhodnocení početnosti z hlediska metodiky
      POCETNOST = case_when(DRUH == "Euplagia quadripunctaria" & POCET_CAT > 1 ~ 1,
                            DRUH == "Euplagia quadripunctaria" & POCET_CAT <= 1 ~ 0,
                            DRUH == "Phengaris nausithous" & POCET_CAT > 1 ~ 1,
                            DRUH == "Phengaris nausithous" & POCET_CAT <= 1 ~ 0,
                            DRUH == "Phengaris teleius" & POCET_CAT > 1 ~ 1,
                            DRUH == "Phengaris teleius" & POCET_CAT <= 1 ~ 0,
                            DRUH == "Euphydryas aurinia" & POCET_CAT > 1 ~ 1,
                            DRUH == "Euphydryas aurinia" & POCET_CAT <= 1 ~ 0,
                            DRUH == "Euphydryas maturna" & POCET_CAT > 1 ~ 1,
                            DRUH == "Euphydryas maturna" & POCET_CAT <= 1 ~ 0),
      # Hodnocení výskytu hostitelských rostliv podle poznámky - řešení pro data, která nejsou nasbírána v souladu s metodikou
      # Pro data nasbíraná podle metodiky bude analýza sahat do strukturované poznámky, která by měla obsahovat data ze Survey123
      MOWING_TIME = case_when(grepl(paste(c("<sec_nacasovani>ne"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                              grepl(paste(c("<sec_nacasovani>ano"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1),
      MOWING_METHOD = case_when(grepl(paste(c("<sec_celoplosna>ano"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                grepl(paste(c("<sec_celoplosna>ne"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1),
      PLANTS = case_when(DRUH == "Phengaris nausithous" & grepl(paste(c("<toten_pritomnost>žádné"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                         DRUH == "Phengaris nausithous" & grepl(paste(c("<toten_pritomnost>jednotlivě"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                         DRUH == "Phengaris nausithous" & grepl(paste(c("<toten_pritomnost>hojně"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1,
                         DRUH == "Phengaris nausithous" & grepl(paste(c("<toten_pritomnost>dominantně"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1,
                         DRUH == "Phengaris teleius" & grepl(paste(c("<toten_pritomnost>žádné"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                         DRUH == "Phengaris teleius" & grepl(paste(c("<toten_pritomnost>jednotlivě"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0,
                         DRUH == "Phengaris teleius" & grepl(paste(c("<toten_pritomnost>hojně"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1,
                         DRUH == "Phengaris teleius" & grepl(paste(c("<toten_pritomnost>dominantně"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 1),
      DESTRUCT = case_when(grepl(paste(c("vysazování lesů", "odvodňování, meliorace",
                                         "zalesňování bezlesí", "změna zemědělského využívání půdy"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0),
      THREATS = str_count(STRUKT_POZN, ","),
      TARGET = case_when(ZDROJ != "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." &
                           ZDROJ != "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." &
                           ZDROJ != "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." &
                           ZDROJ != "Sledování stavu EVL - IPLife" ~ 0,
                         ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
                           ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
                           ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
                           ZDROJ == "Sledování stavu EVL - IPLife" ~ 1)) 
  species <- species %>%
    # Posledních 6 let pro hodnocení
    filter(YEAR >= (current_year - 6))
  return(species)
}

# Hodnocení lokality (hodnocení EVL v konečné verzi bude záviset na velikosti EVL - EVL může obsahovat více jak jednu lokalitu)
Lep.1.evl <- function(species) {
  # Data frame obsahující všechny EVL druhu
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- Lep.1.clear(species) %>% # Aplikace funcḱce GROUP_clear na hrubá data z NDOP 
    filter(TARGET == 0) %>%
    bind_rows(species_site) %>% # Použití kódu a názvu EVL 
    group_by(SITECODE) %>%  # Rozdělení dat z NDOP podle kódu lokality - každá EVL je analyzována zvlášť
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              # Vyhodnocení přítomnosti druhu - pro pozitivní záznam stačí 1 záznam za období
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) == 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE",
                                   min(na.omit(NEGATIVNI)) == 1 ~ "NE"),
              HABITAT = case_when(max(find_evl_TARGETS(SITECODE)) == 1 & 
                                    max(find_evl_HAB_QUAL(SITECODE)) < 0.5 ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 1 &
                                    max(find_evl_HAB_QUAL(SITECODE)) >= 0.5 ~ "DOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 0 &
                                    max(find_evl_HAB_QUAL(SITECODE)) < 0.1 ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 0 &
                                    max(find_evl_HAB_QUAL(SITECODE)) >= 0.1~ "DOSTAČUJÍCÍ",
                                  max(find_evl_HAB_QUAL(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ"),
              ROSTLINY =  "CHYBÍ DATA",
              LIKVIDACE = case_when(max(na.omit(TARGET)) == -Inf ~ "CHYBÍ DATA",
                                    max(na.omit(TARGET)) == 0 ~ "CHYBÍ DATA"),
              NEGATIV = case_when(max(na.omit(TARGET)) == -Inf ~ "CHYBÍ DATA",
                                  max(na.omit(TARGET)) == 0 ~ "CHYBÍ DATA"),
              MANAGEMENT = case_when(max(na.omit(TARGET)) == -Inf ~ "CHYBÍ DATA",
                                     max(na.omit(TARGET)) == 0 ~ "CHYBÍ DATA"),
              MONITOR = case_when(max(na.omit(TARGET)) == -Inf ~ "NEPROBĚHL",
                                  max(na.omit(TARGET)) == 0 ~ "NEPROBĚHL",
                                  max(na.omit(TARGET)) == 1 ~ "PROBĚHL"),
              OVERALL = NA)
  hab_evl_target <- Lep.1.clear(species) %>%
    bind_rows(species_site) %>%
    filter(ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
             ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
             ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." |
             ZDROJ == "Sledování stavu EVL - IPLife") %>%
    group_by(SITECODE, POLE) %>%
    summarise(SITECODE = unique(SITECODE),
              POLE = unique(POLE),
              NAZEV = unique(NAZEV),
              PRESENCE = case_when(min(na.omit(NEGATIVNI)) == 0 ~ 1,
                                   min(na.omit(NEGATIVNI)) == 1 ~ 0),
              ROSTLINY_pre = case_when(mean(na.omit(PLANTS)) == -Inf ~ as.integer(NA),
                                       max(find_evl_TARGETS(SITECODE)) == 1 &
                                         mean(na.omit(filter(., YEAR == current_year)$PLANTS)) < 0.5 ~ as.integer(0),
                                       mean(na.omit(PLANTS)) >= 0.5 ~ as.integer(1),
                                       mean(na.omit(PLANTS)) < 0.5 ~ as.integer(0)),
              LIKVIDACE = case_when(max(na.omit(DESTRUCT)) == 0 ~ 0,
                                    max(na.omit(DESTRUCT)) == -Inf ~ 1),
              NEGATIV = case_when(mean(THREATS) > 0 ~ 0,
                                  mean(THREATS) == 0 ~ 1),
              MANAGEMENT = case_when((max(na.omit(MOWING_TIME)) + max(na.omit(MOWING_METHOD))) == -Inf ~ as.integer(NA),
                                     (mean(na.omit(MOWING_TIME)) + mean(na.omit(MOWING_METHOD))) >= 1 ~ as.integer(1),
                                     (mean(na.omit(MOWING_TIME)) + mean(na.omit(MOWING_METHOD))) < 1~ as.integer(0),
                                     (mean(na.omit(MOWING_TIME)) + mean(na.omit(MOWING_METHOD))) == 0 ~ as.integer(0)),
              OVERALL = NA) %>%
    ungroup() %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = unique(SITECODE),
              NAZEV = unique(NAZEV),
              PRESENCE = case_when(mean(na.omit(PRESENCE)) > 0 ~ "ANO",
                                   mean(na.omit(PRESENCE)) == 0 ~ "NE"),
              HABITAT = case_when(max(find_evl_TARGETS(SITECODE)) == 1 & 
                                    max(find_evl_HAB_QUAL(SITECODE)) < 0.5 ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 1 &
                                    max(find_evl_HAB_QUAL(SITECODE)) >= 0.5 ~ "DOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 0 &
                                    max(find_evl_HAB_QUAL(SITECODE)) < 0.1 ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == 0 &
                                    max(find_evl_HAB_QUAL(SITECODE)) >= 0.1~ "DOSTAČUJÍCÍ",
                                  max(find_evl_HAB_QUAL(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ",
                                  max(find_evl_TARGETS(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ"),
              ROSTLINY = case_when(mean(na.omit(ROSTLINY_pre)) >= 0.5 ~ "DOSTATEČNÝ",
                                   mean(na.omit(ROSTLINY_pre)) < 0.5 ~ "NEDOSTATEČNÝ",
                                   max(na.omit(ROSTLINY_pre)) == -Inf ~ "CHYBÍ DATA"),
              LIKVIDACE = case_when(mean(na.omit(LIKVIDACE)) >= 0.5 ~ "NE",
                                    mean(na.omit(LIKVIDACE)) < 0.5 ~ "ANO"),
              NEGATIV = case_when(mean(NEGATIV) < 0.5 ~ "ANO",
                                  mean(NEGATIV) >= 0.5 ~ "NE"),
              MANAGEMENT = case_when(max(na.omit(MANAGEMENT))== -Inf ~ "CHYBÍ DATA",
                                     mean(na.omit(MANAGEMENT)) >= 0.5 ~ "VHODNÝ",
                                     mean(na.omit(MANAGEMENT)) < 0.5~ "NEVHODNÝ"),
              MONITOR =  "PROBĚHL",
              OVERALL = NA) %>%
    filter(is.na(PRESENCE) == FALSE)
  hab_evl <- bind_rows(hab_evl_target, hab_evl)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

# Tabulka s upravenými názvy a vybarvená podle hodnot
Lep.1.semafor <-  function(species) {
  datatable(species,
            extensions = 'Buttons',
            options = list(
              paging = TRUE,
              searching = TRUE,
              fixedColumns = TRUE,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              buttons = c('csv', 'excel')),
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                         "DOSTATEČNÝ VÝSKYT ŽIVNÝCH ROSTLIN", "PŘÍMÁ LIKVIDACE HABITATU", 
                         "NEGATIVNÍ VLIVY", "VLIV MANAGEMENTU", 
                         "CÍLENÝ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "HABITAT",
                background = styleEqual(c("DOSTAČUJÍCÍ", "NEDOSTAČUJÍCÍ", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "ROSTLINY",
                background = styleEqual(c("DOSTATEČNÝ", "NEDOSTATEČNÝ", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "LIKVIDACE",
                background = styleEqual(c("NE", "ANO", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "NEGATIV",
                background = styleEqual(c("NE", "ANO", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "MANAGEMENT",
                background = styleEqual(c("VHODNÝ", "NEVHODNÝ", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "MONITOR",
                background = styleEqual(c("PROBĚHL", "NEPROBĚHL", "CHYBÍ DATA"), 
                                        c("green", "red", "grey")))
}

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
write.csv(results_quad_phenau, 
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
  
  species_target_sites <- species %>%
    filter(NAZEV == evl_site) %>%
    filter(DRUH == species_code) %>%
    mutate(
      PRESENCE_site = dplyr::case_when(NEGATIVNI == 0 ~ 1,
                                       NEGATIVNI == 1 ~ 0,
                                       POCET > 0 ~ 1,
                                       POCET == 0 ~ 0),
      MOWING_TIME_site = dplyr::case_when(grepl("<sec_nacasovani>ne", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                          grepl("<sec_nacasovani>ano", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      MOWING_METHOD_site = dplyr::case_when(grepl("<sec_celoplosna>ano", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                            grepl("<sec_celoplosna>ne", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      PLANTS_site = dplyr::case_when(grepl("<toten_pritomnost>žádné", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>jednotlivě", STRUKT_POZN, ignore.case = TRUE) ~ 0,
                                     grepl("<toten_pritomnost>hojně", STRUKT_POZN, ignore.case = TRUE) ~ 1,
                                     grepl("<toten_pritomnost>dominantně", STRUKT_POZN, ignore.case = TRUE) ~ 1),
      DESTRUCT_site = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                     "odvodňování, meliorace",
                                                     "zalesňování bezlesí", 
                                                     "změna zemědělského využívání půdy"), collapse = "|"), STRUKT_POZN, ignore.case = TRUE) ~ 0),
      THREATS_site = stringr::str_count(STRUKT_POZN, ","),
      TARGET_MON_site = dplyr::case_when(ZDROJ == "Kolektiv autorů (2017) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2018) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2019) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2020) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Kolektiv autorů (2021) Monitoring totenových modrásků. Monitoring druhů ČR. AOPK ČR." ~ 1,
                                         ZDROJ == "Sledování stavu EVL - IPLife" ~ 1,
                                         TRUE ~ 0)
    ) %>%
    mutate(
      MANAGEMENT_site = dplyr::case_when(MOWING_TIME_site == 1 &
                                           MOWING_METHOD_site == 1 ~ 1,
                                         MOWING_TIME_site == 1 |
                                           MOWING_METHOD_site == 1 ~ 0),
      THREAT_PRESS_site = dplyr::case_when(THREATS_site < 3 ~ 1,
                                           THREATS_site >= 3 ~ 0)
    ) %>%
    dplyr::group_by(POLE) %>%
    dplyr::mutate(
      LAST_TARGET_MON = max(filter(., TARGET_MON_site == 1)$DATE),
      LAST_FIND = max(filter(., PRESENCE_site == 1)$DATE)
    ) %>%
    dplyr::filter(DATE >= 2021 - 6) %>%
    dplyr::summarise(
      DRUH = species_code,
      SITECODE = unique(SITECODE),
      POLE = unique(POLE),
      NAZEV = unique(NAZEV),
      PRESENCE = dplyr::case_when(max(na.omit(PRESENCE_site)) == 1 ~ 1,
                                  max(na.omit(PRESENCE_site)) == 0 ~ 0),
      ROSTLINY = mean(na.omit(PLANTS_site)),
      LIKVIDACE = mean(na.omit(DESTRUCT_site)),
      NEGATIV = mean(THREATS_site),
      MANAGEMENT = mean(MANAGEMENT_site),
      TARGET_MON = dplyr::case_when(max(TARGET_MON_site) == 1 ~ 1,
                                    max(TARGET_MON_site) == 0 ~ 0),
      LAST_TARGET_MON = base::unique(LAST_TARGET_MON),
      LAST_FIND = base::unique(LAST_FIND),
      OVERALL = NA) %>%
    dplyr::ungroup()
  
  species_target_sci <- NA
  
  species_target_sites
  
}

Lep.2.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(EVL, 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                          (is.na(POCET) == FALSE & POCET > 0) | 
                            (is.na(POCITANO) == FALSE & POCET > 0) ~ 1)) 
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

Lep.2.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- Lep.2.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              HABITAT = NA,
              LIKVIDACE = NA,
              NEGATIV = NA,
              MANAGEMENT = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

Lep.2.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                         "STAV HABITATU", "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", 
                         "NEGATIVNÍ VLIVY", "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                        c("green", "red", "grey")))
}

Lep.3.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(EVL, 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                          (is.na(POCET) == FALSE & POCET > 0) | 
                            (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
      POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                              REL_POC == "ojediněle" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                            (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                              REL_POC == "vzácně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                            (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                              REL_POC == "roztroušeně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                            (POCET > 100) | REL_POC == "hojně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
      POCETNOST = case_when(DRUH == "Eriogaster catax" & POCET_CAT > 1 ~ 1,
                            DRUH == "Eriogaster catax" & POCET_CAT <= 1 ~ 0)) 
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

Lep.3.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- Lep.3.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              POCETNOST = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                    max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ",
                                    max(na.omit(POCETNOST)) == 0 ~ "NEDOSTATEČNÁ"),
              HABITAT = NA,
              LIKVIDACE = NA,
              NEGATIV = NA,
              MANAGEMENT = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

Lep.3.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                         "POČETNOST POPULACE", "STAV HABITATU",
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                        c("green", "red", "grey"))) %>%
    formatStyle(columns = "POCETNOST",
                background = styleEqual(c("DOSTATEČNÁ", "NEDOSTATEČNÁ", "CHYBÍ DATA"), 
                                        c("green", "red", "grey")))
}

# Brouci -----
# Carabidae ----
# Carabus hungaricus
CarOpen.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(as.character(EVL), 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
      POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                              REL_POC == "ojediněle" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                            (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                              REL_POC == "vzácně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                            (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                              REL_POC == "roztroušeně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                            (POCET > 100) | REL_POC == "hojně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
      POCETNOST = case_when(POCET_CAT > 1 ~ 1,
                            POCET_CAT <= 1 ~ 0))
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

CarOpen.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- CarOpen.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT[YEAR >= (current_year - 2)])) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT[YEAR >= (current_year - 2)])) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT[YEAR >= (current_year - 2)])) == 0 ~ "NE"),
              HABITAT = NA,
              LIKVIDACE = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

CarOpen.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                         "PŘÍMÁ LIKVIDACE HABITATU", "NEGATIVNÍ VLIVY", "POSLEDNÍ MONITORING",
                         "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
}


CarAqua.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(as.character(EVL), 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
      POCET_CAT = case_when((POCET > 0 & POCET <= 10) | REL_POC == "1-10" | 
                              REL_POC == "ojediněle" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 1,
                            (POCET > 10 & POCET <= 35) | REL_POC == "11-100" | 
                              REL_POC == "vzácně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 2,
                            (POCET > 35 & POCET < 10) | REL_POC == "11-100" | 
                              REL_POC == "roztroušeně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 3,
                            (POCET > 100) | REL_POC == "hojně" & 
                              (POCITANO == "imaga" | POCITANO == "jedinci") ~ 4),
      POCETNOST = case_when(POCET_CAT > 1 ~ 1,
                            POCET_CAT <= 1 ~ 0))
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

CarAqua.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- CarAqua.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              HABITAT = NA,
              VODA = NA,
              LIKVIDACE = NA,
              MANAGEMENT = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

CarAqua.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", "VODNÍ REŽIM", 
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
}

# Saproxyličtí brouci ----
SapOpen.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(as.character(EVL), 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

SapOpen.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- SapOpen.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              HABITAT = NA,
              LIKVIDACE = NA,
              MANAGEMENT = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

SapOpen.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "STAV HABITATU", 
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
}

SapFor.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(EVL, 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1)
    ) 
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

SapFor.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- SapFor.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              HABITAT = NA,
              DREVO = NA,
              LIKVIDACE = NA,
              MANAGEMENT = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

SapFor.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                         "STAV HABITATU", "PŘÍTOMNOST MRVTÉHO DŘEVA", 
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY", 
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
}

RosAlp.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(EVL, 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
      POCETNOST = case_when(POCET >= 100 & SITECODE == "CZ0514243" ~ 1,
                            POCET < 100 & SITECODE == "CZ0514243" ~ 0,
                            POCET >= 10 & SITECODE != "CZ0514243" ~ 1,
                            POCET < 10 & SITECODE != "CZ0514243" ~ 0)
    ) 
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

RosAlp.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- RosAlp.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              POCET = case_when(max(na.omit(POCETNOST)) == -Inf ~ "CHYBÍ DATA",
                                max(na.omit(POCETNOST)) == 1 ~ "DOSTATEČNÁ"),
              HABITAT = NA,
              DREVO = NA,
              LIKVIDACE = NA,
              MANAGEMENT = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

RosAlp.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", 
                         "POČETNOST POPULACE", "STAV HABITATU", "PŘÍTOMNOST MRVTÉHO DŘEVA", 
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY", 
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey"))) %>%
    formatStyle(columns = "POCET",
                background = styleEqual(c("DOSTATEČNÁ", "NEDOSTATEČNÁ", "CHYBÍ DATA"), 
                                        c("green", "red", "grey")))
}

# Graphoderus ----

ColAqua.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(as.character(EVL), 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1))
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

ColAqua.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- ColAqua.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT[YEAR >= current_year - 2])) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT[YEAR >= current_year - 2])) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT[YEAR >= current_year - 2])) == 0 ~ "NE"),
              HABITAT = NA,
              PRUHLEDNOST = NA,
              ZOOPLANKTON = NA,
              LIKVIDACE = NA,
              MANAGEMENT = NA,
              NEGATIV = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

ColAqua.semafor <-  function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU",
                         "STAV HABITATU", "PŘÍTOMNOST ZOOPLANKTONU", "PRŮHLEDNOST VODY",
                         "PŘÍMÁ LIKVIDACE HABITATU", "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "CHYBÍ DATA"), c("green", "grey")))
}

# Vážky ----

Odo.clear <- function(species) {
  species <- species %>%
    mutate(
      DRUH = as.factor(DRUH),
      DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
      YEAR = substring(DATE, 1, 4),
      SITECODE = substr(as.character(EVL), 1, 9),
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      PRESENT = case_when(is.na(POCET) == FALSE & POCET > 0 & is.na(POCITANO) == TRUE ~ 0,
                          is.na(POCET) == FALSE | POCET > 0 | is.na(POCITANO) == FALSE ~ 1),
      POCETNOST = case_when(POCET > 10 ~ 1,
                            POCET <= 10 ~ 0))
  species <- species %>%
    filter(YEAR >= (current_year - 6))
  return(species)
}

Odo.site <- function(species) {
  species_site <- as.data.frame(cbind(as.vector(find_evl_SITECODE(input_species)), 
                                      as.vector(find_evl_NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- Odo.clear(species) %>%
    bind_rows(species_site) %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = SITECODE,
              NAZEV = NAZEV,
              PRESENCE = case_when(max(na.omit(PRESENT)) == -Inf ~ "CHYBÍ DATA",
                                   max(na.omit(PRESENT)) >= 1 ~ "ANO",
                                   max(na.omit(PRESENT)) == 0 ~ "NE"),
              REPRO = NA,
              HABITAT = NA,
              LIKVIDACE = NA,
              NEGATIV = NA,
              MANAGEMENT = NA,
              MONITOR = NA,
              OVERALL = NA)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

Odo.semafor <- function(species) {
  datatable(species, 
            rownames = FALSE,
            filter = "top",
            colnames = c("KÓD EVL", "NÁZEV EVL", "PŘÍTOMNOST DRUHU", "ZAZNAMENÁNÍ REPRODUKCE", 
                         "STAV HABITATU", "PŘÍMÁ LIKVIDACE HABITATU", 
                         "VLIV MANAGEMENTU", "NEGATIVNÍ VLIVY",
                         "POSLEDNÍ MONITORING", "CELKOVÉ HODNOCENÍ")) %>%
    formatStyle(columns = "PRESENCE",
                background = styleEqual(c("ANO", "NE", "CHYBÍ DATA"), 
                                        c("green", "red", "grey")))
}

# Stenobothrus ----
