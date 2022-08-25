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
BuxVir.clear <- function(species) {
  species <- species %>%
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
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      # Přítomnost druhu
      PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                          (is.na(POCET) == FALSE & POCET > 0) | 
                            (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
      # Vyhodnocení početnosti z hlediska metodiky
      POCETNOST = case_when(DRUH == "Buxbaumia viridis" & POCET >= 3 ~ 1,
                            DRUH == "Buxbaumia viridis" & POCET < 3 ~ 0),
      DREVO = case_when(parse_number(STRUKT_POZN) >= 3 ~ 1,
                        parse_number(STRUKT_POZN) < 3 ~ 0),
      THREATS = str_count(STRUKT_POZN, ","),
      TARGET = case_when(PROJEKT != "Monitoring druhů ČR" ~ 0,
                         PROJEKT == "Monitoring druhů ČR" ~ 1)) 
  species <- species %>%
    # Posledních 6 let pro hodnocení
    filter(YEAR >= (current_year - 6))
  return(species)
}

BuxVir.evl <- function(species) {
  # Data frame obsahující všechny EVL druhu
  species_site <- as.data.frame(cbind(as.vector(find.evl.SITECODE(input_species)), 
                                      as.vector(find.evl.NAZEV(input_species))))
  colnames(species_site) <- c("SITECODE", "NAZEV")
  hab_evl <- BuxVir.clear(species) %>% # Aplikace funcḱce GROUP_clear na hrubá data z NDOP 
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
              HABITAT = case_when(max(find.evl.TARGETS(SITECODE)) == 1 & 
                                    max(find.evl.HAB_QUAL(SITECODE)) < 0.5 ~ "NEDOSTAČUJÍCÍ",
                                  max(find.evl.TARGETS(SITECODE)) == 1 &
                                    max(find.evl.HAB_QUAL(SITECODE)) >= 0.5 ~ "DOSTAČUJÍCÍ",
                                  max(find.evl.TARGETS(SITECODE)) == 0 &
                                    max(find.evl.HAB_QUAL(SITECODE)) < 0.1 ~ "NEDOSTAČUJÍCÍ",
                                  max(find.evl.TARGETS(SITECODE)) == 0 &
                                    max(find.evl.HAB_QUAL(SITECODE)) >= 0.1~ "DOSTAČUJÍCÍ",
                                  max(find.evl.HAB_QUAL(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ",
                                  max(find.evl.TARGETS(SITECODE)) == -Inf ~ "NEDOSTAČUJÍCÍ"),
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
  hab_evl_target <- BuxVir.clear(species) %>%
    bind_rows(species_site) %>%
    filter(PROJEKT == "Monitoring druhů ČR" |
             PROJEKT == "Sledování stavu EVL - IPLife") %>%
    group_by(SITECODE, POLE) %>%
    summarise(SITECODE = unique(SITECODE),
              POLE = unique(POLE),
              NAZEV = unique(NAZEV),
              PRESENCE = case_when(mean(na.omit(PRESENCE)) > 0 ~ "ANO",
                                   mean(na.omit(PRESENCE)) == 0 ~ "NE"),
              POCETNOST = NA,
              DREVO = NA,
              NEGATIV = NA,
              OVERALL = NA) %>%
    ungroup() %>%
    group_by(SITECODE) %>%
    summarise(SITECODE = unique(SITECODE),
              NAZEV = unique(NAZEV),
              PRESENCE = case_when(mean(na.omit(PRESENCE)) > 0 ~ "ANO",
                                   mean(na.omit(PRESENCE)) == 0 ~ "NE"),
              POCETNOST = NA,
              DREVO = NA,
              NEGATIV = NA,
              MONITOR =  "PROBĚHL",
              OVERALL = NA) %>%
    filter(is.na(PRESENCE) == FALSE)
  hab_evl <- bind_rows(hab_evl_target, hab_evl)
  hab_evl <- hab_evl[hab_evl$SITECODE %in% species_site$SITECODE,]
  hab_evl <- hab_evl[!duplicated(hab_evl$SITECODE),]
  return(hab_evl)
}

BuxVir.semafor <-  function(species) {
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

DicVir.clear <- function(species) {
  species <- species %>%
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
      NAZEV = substr(as.character(EVL), 12, nchar(as.character(EVL))),
      # Přítomnost druhu
      PRESENT = case_when(is.na(POCET) == TRUE | is.na(POCITANO) == TRUE ~ 0,
                          (is.na(POCET) == FALSE & POCET > 0) | 
                            (is.na(POCITANO) == FALSE & POCET > 0) ~ 1),
      # Vyhodnocení početnosti z hlediska metodiky
      POCET_MIKRO = case_when(parse_number(STRUKT_POZN) >= 3 ~ 1,
                              POCET < 3 ~ 0),
      DREVO = case_when(parse_number(STRUKT_POZN) >= 3 ~ 1,
                        parse_number(STRUKT_POZN) < 3 ~ 0),
      THREATS = str_count(STRUKT_POZN, ","),
      TARGET = case_when(PROJEKT != "Monitoring druhů ČR" ~ 0,
                         PROJEKT == "Monitoring druhů ČR" ~ 1)) 
  species <- species %>%
    # Posledních 6 let pro hodnocení
    filter(YEAR >= (current_year - 6))
  return(species)
}