# SET WD ----
setwd("~/N2K.CZ-main")
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

if(!isTRUE(require(matrixStats, quietly = TRUE))) {
  install.packages("matrixStats", dependencies = TRUE); library(matrixStats)
} else {
  require(matrixStats)}

if(!isTRUE(require(ggradar, quietly = TRUE))) {
  install.packages("ggradar", dependencies = TRUE); library(ggradar)
} else {
  require(ggradar)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(lubridate, quietly = TRUE))) {
  install.packages("lubridate", dependencies = TRUE); library(lubridate)
} else {
  require(lubridate)}

if(!isTRUE(require(condformat, quietly = TRUE))) {
  install.packages("condformat", dependencies = TRUE); library(condformat)
} else {
  require(condformat)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(raster, quietly = TRUE))) {
  install.packages("raster", dependencies = TRUE); library(raster)
} else {
  require(raster)}

if(!isTRUE(require(stars, quietly = TRUE))) {
  install.packages("stars", dependencies = TRUE); library(stars)
} else {
  require(stars)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(zoo, quietly = TRUE))) {
  install.packages("zoo", dependencies = TRUE); library(zoo)
} else {
  require(zoo)}

if(!isTRUE(require(rgdal, quietly = TRUE))) {
  install.packages("rgdal", dependencies = TRUE); library(rgdal)
} else {
  require(rgdal)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(grid, quietly = TRUE))) {
  install.packages("grid", dependencies = TRUE); library(grid)
} else {
  require(grid)}

if(!isTRUE(require(gridGraphics, quietly = TRUE))) {
  install.packages("gridGraphics", dependencies = TRUE); library(gridGraphics)
} else {
  require(gridGraphics)}

if(!isTRUE(require(DT, quietly = TRUE))) {
  install.packages("DT", dependencies = TRUE); library(DT)
} else {
  require(DT)}


# LOAD DATA ----
# VRSTVA EVL
evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
#evl <- sf::st_read("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/Evropsky_významné_lokality.geojson")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))
# HRANICE ČR
czechia <- st_read("HraniceCR.shp")
czechia_line <- st_cast(czechia, "LINESTRING")
# BIOGEOGRAFICKÉ ČLENĚNÍ ČR
#bioregs <- st_read("BiogeoRegions_CR.shp")
#bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))
# VMB - MÁJOVÁ VRSTVA 2022
vmb_shp_sjtsk_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/20220531_Segment.shp")
vmb_hab_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/HAB_BIOTOP.dbf")

vmb_shp_sjtsk_22 <- vmb_shp_sjtsk_22 %>%
  dplyr::left_join(vmb_hab_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(STEJ_PR < 50 ~ "X",
                                            STEJ_PR >= 50 & STEJ_PR < 100 ~ "moz.",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP %in% c("T3.3C", "3.4A", "T3.4C", "T3.5A") ~ "6210p",
                                           TRUE ~ HABITAT))

# VMB - MÁJOVÁ VRSTVA 2021
vmb_shp_sjtsk_21 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2021/20210610_Segment.shp")
vmb_hab_dbf_21 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2021/Biotop/HAB_BIOTOP.dbf")
vmb_shp_sjtsk_21 <- vmb_shp_sjtsk_21 %>%
  dplyr::left_join(vmb_hab_dbf_21, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(STEJ_PR < 50 ~ "X",
                                            STEJ_PR >= 50 & STEJ_PR < 100 ~ "moz.",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP %in% c("T3.3C", "3.4A", "T3.4C", "T3.5A") ~ "6210p",
                                           TRUE ~ HABITAT))



# SEZNAM EVL A PŘEDMĚTŮ OCHRANY
sites_subjects <- openxlsx::read.xlsx("https://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000_440_2021.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")
sites_habitats <- sites_subjects %>%
  filter(Typ.předmětu.ochrany == "stanoviště")
# ČÍSELNÍK HABITATŮ
habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
# SEZNAM MINIMIAREÁLŮ
minimisize <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/minimisize.csv", encoding = "UTF-8")
# rozloze stanoviště v ČR v rámci AVMB2022
habitat_areas_2022 <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitat_areas_2022.csv", encoding = "UTF-8")
# MAXIMÁLNÍ VZDÁLENOST MEZI 2 BODY PRO KAŽDOU EVL - LINESTRINGY BYLY PŘEVEDENY NA MULTIPOINT 
# PRO EVL S OBVODEM < 10 KM BYLY POUŽITY VŠECHNY BODY, PRO VĚTŠÍ EVL KAŽDÝ SEDMÝ
evl_lengths <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_max_dist.csv", encoding = "UTF-8")
#evl_species <- read.csv("https://media.githubusercontent.com/media/jonasgaigr/N2K.CZ/main/cevnate_evl.csv", encoding = "UTF-8")
# REDLISTOVÉ, INVAZNÍ A EXPANZNÍ DRUHY
evl_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_cev_rostliny_EVL_21062022.xlsx")
# STAŽENÍ PŘES GDRIVE "https://docs.google.com/spreadsheets/d/12n1eCbkw8ufsFoUnKvEMMg123bfMFOzI/edit?usp=sharing&ouid=102654749342263703541&rtpof=true&sd=true"
evl_expansive_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_expanzivky_EVL_21062022.xlsx")

# EXPANZNÍ DRUHY
expansive_species <- evl_expansive_species %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  filter(DRUH != "Arrhenatherum elatius") %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# DRUHY ČERVENÉHO SEZNAMU S ODFILTROVANÝMI C4 DRUHY
red_list_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == TRUE) %>%
  filter(is.na(Druhy.červeného.seznamu) == FALSE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# INVAZNÍ DRUHY
invasive_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == FALSE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# LIMITNÍ HODNOTY PARAMETRŮ HODNOCENÍ
hablimits <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/hablimits.csv", encoding = "UTF-8") %>%
  filter(REG != "alp" | is.na(REG) == TRUE)

# N2K.CZ FUNKCE ----
# FUNKCE USNADŇUJÍCÍ PRÁCI S PŘEDMĚTY OCHRANY
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
# NÁZEV EVL PODLE KÓDU
find_evl_CODE_TO_NAME <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Kód.lokality == species) %>%
           dplyr::pull(Název.lokality) %>%
           unique()
  )
}
# NÁZVEV HABITATU PODLE KÓDU
find_habitat_NAME_CZ <-function(species) {
  return(habitats %>%
           dplyr::filter(HABITAT_CODE == species) %>%
           dplyr::pull(HABITAT_CZ) %>%
           unique()
  )
}
# KÓD HABITATU PODLE NÁZVU
find_habitat_CODE <-function(species) {
  return(habitats %>%
           dplyr::filter(HABITAT_CZ == species) %>%
           dplyr::pull(HABITAT_CODE) %>%
           unique()
  )
}
# MINIMIAREÁL HABITATU
find_habitat_MINIMISIZE <- function(species) {
  return(minimisize %>%
           dplyr::filter(HABITAT == species) %>%
           pull(MINIMISIZE) %>%
           unique()
  )
}
# ROZLOHA HABITATU 2022
find_habitat_AREA2022 <- function(species) {
  return(habitat_areas_2022 %>%
           dplyr::filter(HABITAT == species) %>%
           pull(TOTAL_AREA) %>%
           unique()
  )
}
# PRIORITA OCHRANY HABITATU
find_evl_PRIORITY <- function(species) {
  return(minimisize %>%
           dplyr::filter(HABITAT == species) %>%
           pull(PRIORITA) %>%
           unique()
  )
}
# LIMITNÍ HODNOTY PARAMETRŮ HODNOCENÍ HABITATU
find_habitat_LIMIT <- function(species) {
  return(hablimits %>%
           dplyr::filter(HABITAT_CODE == species)
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


# VÝPOČET HODNOCENÍ ----
hvezdice_eval <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_22 %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
    dplyr::filter(HABITAT == hab_code) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION POINT") %>%
    dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
    dplyr::mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA)
  
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk_22 %>%
    sf::st_filter(., evl_sjtsk %>%
                    dplyr::filter(., SITECODE == evl_site) %>%
                    sf::st_buffer(., 500)) %>%
    dplyr::filter(FSB_EVAL != "X" &
                    FSB_EVAL != "-" &
                    FSB_EVAL != "-1" &
                    HABITAT != hab_code) %>% 
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) %>%
    dplyr::group_by(SEGMENT_ID_buff) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # CELKOVÁ PLOCHA HABITATU VČETNĚ PASEK
  area_paseky_ha <- find_habitat_PASEKY(evl_site, hab_code)
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk %>%
                       dplyr::pull(PLO_BIO_M2_EVL) %>%
                       sum(),
                     area_paseky_ha*10000,
                     na.rm = TRUE)
  
  target_area_ha <- SUM_PLO_BIO/10000
  
  area_w_ha <- vmb_target_sjtsk %>%
    dplyr::filter(DG == "W" | RB == "W") %>%
    pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/10000
  area_w_perc <- area_w_ha/target_area_ha*100
  
  area_paseky_perc <- area_paseky_ha/target_area_ha*100
  
  area_degrad_ha <- sum(area_w_ha, area_paseky_ha, na.rm = TRUE)
  area_degrad_perc <- area_degrad_ha/target_area_ha*100
  
  SUM_PLO_BIO_MINIMI <- sum(vmb_target_sjtsk %>%
                              dplyr::filter(DG != "W" & RB != "W") %>%
                              dplyr::pull(PLO_BIO_M2_EVL) %>%
                              sum())
  
  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_qual <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                       RB == "W" ~ 0,
                                       TD == "N" ~ 0,
                                       TD == "MP" ~ 1,
                                       TD == "P" ~ 2),
      REPREZENTAVITA_SEG = dplyr::case_when(DG == "W" ~ 0,
                                            RB == "W" ~ 0,
                                            RB == "F" ~ 0,
                                            RB == "P" ~ 1,
                                            RB == "V" ~ 2),
      REPRESENTAVITY_SEG = dplyr::case_when(DG == "W" ~ "D",
                                            RB == "V" & TD == "P" ~ "A",
                                            RB == "V" & TD == "MP" ~ "B",
                                            RB == "V" & TD == "N" ~ "C",
                                            RB == "V" & is.na(TD) == TRUE ~ "A",
                                            RB == "P" & TD == "P" ~ "B",
                                            RB == "P" & TD == "MP" ~ "C",
                                            RB == "P" & TD == "N" ~ "C",
                                            RB == "P" & is.na(TD) ~ "B",
                                            RB == "F" & TD == "P" ~ "C",
                                            RB == "F" & TD == "MP" ~ "C",
                                            RB == "F" & TD == "N" ~ "C",
                                            RB == "F" & is.na(TD) ~ "C",
                                            RB == "W" & TD == "P" ~ "D",
                                            RB == "W" & TD == "MP" ~ "D",
                                            RB == "W" & TD == "N" ~ "D",
                                            RB == "W" & is.na(TD) ~ "D",
                                            is.na(RB) == TRUE & TD == "P" ~ "B",
                                            is.na(RB) == TRUE & TD == "MP" ~ "C",
                                            is.na(RB) == TRUE & TD == "N" ~ "C",
                                            is.na(RB) == TRUE & is.na(TD) ~ "C"),
      CONSERVATION_SEG = dplyr::case_when(SF == "P" ~ "A",
                                          SF == "MP" ~ "B",
                                          SF == "N" ~ "C",
                                          RB == "W" ~ "C",
                                          is.na(SF) == TRUE & DG == 0 ~ "A",
                                          is.na(SF) == TRUE & DG == 1 ~ "A",
                                          is.na(SF) == TRUE & DG == 2 ~ "B",
                                          is.na(SF) == TRUE & DG == 3 ~ "C",
                                          is.na(SF) == TRUE & is.na(DG) == TRUE ~ "B"),
      REPRE_SDF_SEG = dplyr::case_when(REPRESENTAVITY_SEG == "D" ~ 0,
                                       REPRESENTAVITY_SEG == "C" ~ 33.333333333333333333333,
                                       REPRESENTAVITY_SEG == "B" ~ 66.666666666666666666666,
                                       REPRESENTAVITY_SEG == "A" ~ 100),
      CON_SEG = dplyr::case_when(CONSERVATION_SEG == "D" ~ 0,
                                 CONSERVATION_SEG == "C" ~ 33.333333333333333333333,
                                 CONSERVATION_SEG == "B" ~ 66.666666666666666666666,
                                 CONSERVATION_SEG == "A" ~ 100),
      DEGREEOFCONS_SEG = dplyr::case_when(DG == "W" ~ 0,
                                          RB == "W" ~ 0,
                                          SF == "N" ~ 0,
                                          SF == "MP" ~ 100,
                                          SF == "P" ~ 100),
      KVALITA_SEG = dplyr::case_when(DG == "W" ~ 0,
                                     RB == "W" ~ 0,
                                     is.na(KVALITA) == TRUE ~ 0,  
                                     KVALITA == 0 ~ 0,
                                     KVALITA == 1 ~ 3,
                                     KVALITA == 2 ~ 2,
                                     KVALITA == 3 ~ 1,
                                     KVALITA == 4 ~ 0),
      MRTVE_DREVO_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                         DG == "W" ~ 0,
                                         RB == "W" ~ 0,
                                         MD == 0 ~ 0,
                                         MD == 1 ~ 1,
                                         MD == 2 ~ 2,
                                         MD == 3 ~ 0,
                                         MD == 4 ~ 0),
      KALAMITA_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                      DG == "W" ~ 0,
                                      RB == "W" ~ 0,
                                      MD == 0 ~ 0,
                                      MD == 1 ~ 0,
                                      MD == 2 ~ 0,
                                      MD == 3 ~ 10,
                                      MD == 4 ~ 10),
      TD_SEG = TYP_DRUHY_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      RB_SEG = REPREZENTAVITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      RB_SDF_SEG = REPRE_SDF_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      CS_SEG = CON_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      DC_SEG = DEGREEOFCONS_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      CN_SEG = CON_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      QUAL_SEG = KVALITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      MD_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ MRTVE_DREVO_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO),
      KAL_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                 TRUE ~ KALAMITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO)
                  ) %>%
    dplyr::mutate(
      # TYPICKÉ DRUHY
      TD_FIN = sum(na.omit(TD_SEG)) + 1,
      # REPREZENTATIVITA
      RB_FIN = sum(na.omit(RB_SEG)) + 1,
      # REPREZENTATIVITA SDF
      RB_SDF_FIN = sum(na.omit(RB_SDF_SEG)),
      # DEGREE OF CONSERVATION
      DC_FIN = sum(na.omit(DC_SEG)),
      # CONSERVATION
      CN_FIN = sum(na.omit(CN_SEG)),
      # MRTVÉ DŘEVO
      MD_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(na.omit(MD_SEG)) + 1),
      # KALAMITA A POLOM
      KP_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(na.omit(KAL_SEG))),
      # KVALITA
      QUALITY = 4 - sum(na.omit(QUAL_SEG)))
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= find_habitat_MINIMISIZE(hab_code)[1] ~ 1,
                                          COMB_SIZE < find_habitat_MINIMISIZE(hab_code)[1] ~ 0)) %>%
    dplyr::ungroup()
  
  if(nrow(vmb_target_sjtsk %>% dplyr::filter(DG != "W" & RB != "W")) > 0) {
    
    celistvost_minimi <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL) %>%
      sum()
    celistvost <- celistvost_minimi/SUM_PLO_BIO*100
    
    celistvost_num <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(ID_COMB) %>%
      base::unique() %>%
      base::length()
      
  } else {
    celistvost <- NA
    celistvost_num <- NA
  }
  
  # PARAMETR MOZAIKA
  spat_union <- vmb_spat %>%
    sf::st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number())
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  if(nrow(vmb_spat) > 0) {
    border_nat <- spat_union %>%
      sf::st_intersection(., vmb_buff) %>%
      dplyr::filter(!SEGMENT_ID_buff %in% vmb_spat$SEGMENT_ID) %>%
      dplyr::group_by(SEGMENT_ID_buff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
    border_all <- spat_union %>%
      sf::st_cast(., "LINESTRING") %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
    border_hsl <- spat_union %>%
      sf::st_intersection(., czechia_line) %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # VÝPOČET PARAMETRU MOZAIKA
    mozaika_bord <- 1 - border_hsl/border_all
    
    mozaika <- border_nat/(border_all-border_hsl)*mozaika_bord*100

    if(mozaika > 100) {
      mozaika <- 100
    }
    
  } else {
    mozaika <- NA
    mozaika_bord <- NA
  }
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_qual %>%
    dplyr::filter(grepl("X", BIOTOP_SEZ, ignore.case = TRUE)) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/SUM_PLO_BIO*100
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  

  if (is.na(perc_spat) == TRUE | is.null(perc_spat) == TRUE) {
    mozaika_kompil <- NA
  } else if (perc_spat >= 25) {
    mozaika_kompil <- mozaika
  } else {
    mozaika_kompil <- 100 - mozaika_inner
  } 
  
  area_evl_perc <- target_area_ha/(unique(vmb_target_sjtsk$SHAPE_AREA.1)/10000)*100
  area_relative_perc <- target_area_ha/(find_habitat_AREA2022(hab_code)/10000)*100
  
  area_good_ha <- vmb_target_sjtsk %>%
    dplyr::filter(SF == "P" | SF == "MP") %>%
    pull(PLO_BIO_M2_EVL) %>%
    sum()/10000
  
  if(nrow(vmb_target_sjtsk) == 0) {
    target_area_ha <- 0
    area_w_ha <- 0
    area_w_perc <- 0
    area_evl_perc <- 0
    area_good_ha <- 0
  }
  
  # VYPLNĚNOST PARAMETRŮ
  fill_TD <- sum(filter(vmb_target_sjtsk, is.na(TD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_QUAL <- sum(filter(vmb_target_sjtsk, is.na(KVALITA) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_MD <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_KP <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  
  # RED LIST SPECIES
  redlist_list <- red_list_species %>%
    sf::st_filter(., vmb_spat) %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  redlist <- redlist_list %>%
    length()/log(sum(vmb_spat$PLO_BIO_M2_EVL))*4
  
  if(redlist > 10) {
    redlist <- 10
  } 
  
  if(length(redlist_list) == 0) {
    redlist_list <- NA
  } 
  
  if(nrow(vmb_spat) == 0) {
    redlist_list <- NA
    redlist <- NA
  }
  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }
  
  invaders_calc <- invaders_all %>%
    dplyr::group_by(OBJECTID.y) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  invaders_list <- invaders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
    
  
  invaders <- sum(invaders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE)*100
  
  if(length(invaders_list) == 0 |
     nrow(vmb_target_sjtsk) == 0) {
    invaders_list <- NA
  }
  
  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVNOST %in% c("3", "4", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.x) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders_calc <- expanders_all %>%
    dplyr::group_by(OBJECTID.y) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  expanders_list <- expanders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  expanders <- sum(expanders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE)*100
  
  
  if(length(expanders_list) == 0 &
     nrow(vmb_target_sjtsk) == 0) {
    expanders <- NA
    expanders_list <- NA
  } else if (length(expanders_list) == 0 &
               nrow(vmb_target_sjtsk) > 0) {
    expanders <- 0
    expanders_list <- NA
  }
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_spat <- NA
  }
  
  vmb_target_date <- vmb_target_sjtsk %>%
    pull(DATUM.x)
  
  min_date <- vmb_target_date %>%
    min() %>%
    unique()
  
  max_date <- vmb_target_date %>%
    max() %>%
    unique()
  
  mean_date <- mean(vmb_target_date)
  
  median_date <- median(vmb_target_date)
  
  perc_seg_0 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y == 0) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_1 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y > 0 & ROK_AKT.y <= 2012) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_2 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y > 2012 & ROK_AKT.y <= 2024) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_seg_0 <- NA
    perc_seg_1 <- NA
    perc_seg_2 <- NA
  }
  
  
  # VÝSLEDKY
  if(target_area_ha > 0 & is.na(target_area_ha) == FALSE) {
    result <- vmb_qual %>%
      dplyr::summarise(SITECODE = unique(SITECODE),
                       NAZEV = unique(NAZEV),
                       HABITAT_CODE = unique(HABITAT),
                       ROZLOHA = target_area_ha,
                       KVALITA = unique(QUALITY),
                       TYPICKE_DRUHY = unique(TD_FIN),
                       REPRE = unique(RB_FIN),
                       REPRE_SDF = unique(RB_SDF_FIN),
                       CONSERVATION = unique(CN_FIN),
                       DEGREE_OF_CONSERVATION = unique(DC_FIN),
                       MINIMIAREAL = celistvost,
                       MINIMIAREAL_JADRA = celistvost_num,
                       MOZAIKA_VNEJSI = mozaika,
                       MOZAIKA_VNITRNI = (100 - mozaika_inner),
                       MOZAIKA_FIN = mozaika_kompil,
                       RED_LIST = redlist,
                       INVASIVE = (100 - invaders),
                       EXPANSIVE = (100 - expanders),
                       MRTVE_DREVO = unique(MD_FIN),
                       KALAMITA_POLOM = unique(KP_FIN),
                       RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                       INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                       EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
                       RELATIVE_AREA_PERC = area_relative_perc,
                       EVL_AREA_PERC = area_evl_perc,
                       GOOD_DOC_AREA_HA = area_good_ha,
                       W_AREA_HA = area_w_ha,
                       W_AREA_PERC = area_w_perc,
                       PASEKY_AREA_HA = area_paseky_ha,
                       PASEKY_AREA_PERC = area_paseky_perc,
                       DEGRAD_AREA_HA = area_degrad_ha,
                       DEGRAD_AREA_PERC = area_degrad_perc,
                       VYPLNENOST_TD = fill_TD,
                       VYPLNENOST_KVALITA = fill_QUAL,
                       VYPLNENOST_MD = fill_MD,
                       VYPLNENOST_MOZAIKA = mozaika_bord,
                       PERC_SPAT = perc_spat,
                       PERC_0 = perc_seg_0,
                       PERC_1 = perc_seg_1,
                       PERC_2 = perc_seg_2,
                       DATE_MIN = min_date,
                       DATE_MAX = max_date,
                       DATE_MEAN = mean_date,
                       DATE_MEDIAN = median_date
      )
  } else {
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = 0,
                     KVALITA = NA,
                     TYPICKE_DRUHY = NA,
                     REPRE = NA,
                     REPRE_SDF = NA,
                     CONSERVATION = NA,
                     DEGREE_OF_CONSERVATION = NA,
                     MINIMIAREAL = NA,
                     MINIMIAREAL_JADRA = NA,
                     MOZAIKA_VNEJSI = NA,
                     MOZAIKA_VNITRNI = NA,
                     MOZAIKA_FIN = NA,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     MRTVE_DREVO = NA,
                     KALAMITA_POLOM = NA,
                     RED_LIST_SPECIES = NA,
                     INVASIVE_LIST = NA,
                     EXPANSIVE_LIST = NA,
                     RELATIVE_AREA_PERC = NA,
                     EVL_AREA_PERC = NA,
                     GOOD_DOC_AREA_HA = NA,
                     W_AREA_HA = NA,
                     W_AREA_PERC = NA,
                     PASEKY_AREA_HA = NA,
                     PASEKY_AREA_PERC = NA,
                     DEGRAD_AREA_HA = NA,
                     DEGRAD_AREA_PERC = NA,
                     VYPLNENOST_TD = NA,
                     VYPLNENOST_KVALITA = NA,
                     VYPLNENOST_MD = NA,
                     VYPLNENOST_MOZAIKA = NA,
                     PERC_SPAT = NA,
                     PERC_0 = NA,
                     PERC_1 = NA,
                     PERC_2 = NA,
                     DATE_MIN = NA,
                     DATE_MAX = NA,
                     DATE_MEAN = NA,
                     DATE_MEDIAN = NA)
  }
  
  return(result)
  
}

# VYKRESLENÍ HVĚZDICOVÉHO GRAFU ----
hvezdice_plot <- function(habresult) {
  habresult <- as.data.frame(habresult)
  result_area <- paste("Rozloha stanoviště:", 
                       format(round(habresult[1,4], 3), nsmall = 3), 
                       "ha", 
                       sep = " ")
  result_habitat_cz = case_when(habresult[1,3] == "91F0" ~ "Smíšené lužní lesy s dubem letním (Quercus robur), jilmem vazem (Ulmus laevis), j. habrolistým (U. minor), jasanem ztepilým\n(Fraxinus excelsior) nebo j. úzkolistým (F. angustifolia) podél velkých řek atlantské a středoevropské provincie (Ulmenion minoris)",
                                habresult[1,3] == "3130" ~ "Oligotrofní až mezotrofní stojaté vody nížinného až subalpínského stupně kontinentální a alpínské oblasti\na horských poloh a jiných oblastí, s vegetací tříd Littorelletea uniflorae nebo Isoëto-Nanojuncetea",
                                habresult[1,3] == "6230" ~ "Druhově bohaté smilkové louky na silikátových podložích v horských oblastech (a v kontinentální\nEvropě v podhorských oblastech)",
                                habresult[1,3] != "91F0" |
                                  habresult[1,3] == "3130" |
                                  habresult[1,3] == "6230" ~ find_habitat_NAME_CZ(habresult[1,3]))
  result_fill_TD <- paste("Vyplněnost parametru 'typické druhy': ", habresult[1,17]*100, "%", sep = "")
  result_fill_QUAL <- paste("Vyplněnost parametru 'kvalita': ", paste(format(round(habresult[1,18]*100, 0), nsmall = 0)), "%", sep = "")
  result_fill <- paste(result_fill_TD, result_fill_QUAL, sep = "; ")
  result_redlist <- paste("Red list:", 
                          format(round(habresult[1,11], 3), nsmall = 3), 
                          sep = " ")
  result_limits <- habresult %>% 
    dplyr::select(TD_LIM,
                  QUAL_LIM, 
                  MINIMIAREAL_LIM, 
                  MOZAIKA_LIM, 
                  CELISTVOST_LIM, 
                  KONEKTIVITA_LIM, 
                  INVASIVE_LIM, 
                  EXPANSIVE_LIM)
  colnames(result_limits) <- colnames(habresult[,c(4:10, 12:13)])
  plot_habresult <- dplyr::bind_rows(result_limits, habresult[,c(4:10, 12:13)])
  ggradar::ggradar(plot_habresult[2,c(1:9)],
                   values.radar = c("0", "5", "10"),
                   grid.min = 0, 5, grid.mid = 5, grid.max = 10.01,
                   group.line.width = 2.5, 
                   group.point.size = 3,
                   axis.label.size = 5, 
                   group.colours = c("#40c8f8", "#f87040"),
                   background.circle.colour = "white",
                   grid.line.width = 1.5,
                   gridline.mid.colour = "grey",
                   gridline.max.colour = "grey",
                   axis.line.colour = "light grey") +
    labs(title = c(paste(habresult[1,2], habresult[1,3], sep = " ")),
         subtitle = c(paste(result_habitat_cz,
                            result_area, 
                            result_redlist,
                            result_fill_TD,
                            result_fill_QUAL,
                            sep = "\n"))) +
    theme(plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          legend.position = "none")
}


# RESULTS ----
hu <- hvezdice_eval(sites_habitats[38,5], sites_habitats[38,1])

habresults_100_110 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 95:110) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,1])))
}

habresults_1_500 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1_500) <- colnames(hu)
habresults_501_1000 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_501_1000) <- colnames(hu)
habresults_1001_1500 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1001_1500) <- colnames(hu)
habresults_1501_1893 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1501_1893) <- colnames(hu)
habresults_901_1893 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_901_1893) <- colnames(hu)

for(i in 1:500) {
  habresults_1_500 <- dplyr::bind_rows(habresults_1_500, 
                                       as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1_500, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/habresults22_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_501_1000 <- dplyr::bind_rows(habresults_501_1000, 
                                          as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_501_1000, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/habresults22_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_1001_1500 <- dplyr::bind_rows(habresults_1001_1500, 
                                           as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1001_1500, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/habresults22_1001_1500.csv", 
           row.names = FALSE)
for(i in 1500:nrow(sites_habitats)) {
  habresults_1501_1893 <- dplyr::bind_rows(habresults_1501_1893, 
                                           as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1501_1893, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/habresults22_1501_1893.csv", 
           row.names = FALSE)

#write.csv2(habresults_1_900, "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_1.csv", row.names = FALSE)
#write.csv2(habresults_901_1893, "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_2.csv", row.names = FALSE)
results_habitats <- bind_rows(habresults_1_500[c(2:nrow(habresults_1_500)),], 
                              habresults_501_1000[c(2:nrow(habresults_501_1000)),],
                              habresults_1001_1500[c(2:nrow(habresults_1001_1500)),],
                              habresults_1501_1893[c(2:nrow(habresults_1501_1893)),])
write.csv2(results_habitats, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_20220926.csv", 
           row.names = FALSE)
results_habitats_read <-  read.csv2("C:/Users/jonas.gaigr/N2K.CZ/results/results_habitats_20221004.csv",
                                    fileEncoding = "Windows-1250")
results_habitats_read[is.na(results_habitats_read)] <- 0

# NAPOJENÍ NA PASEKY ----
results_habitats_read <- read.xlsx("C:/Users/jonas.gaigr/N2K.CZ/results/results_habitats_20220927.xlsx") %>%
  dplyr::select(-DATE_MIN, -DATE_MAX, -DATE_MEDIAN, -DATE_MEAN) %>%
  dplyr::mutate(W_AREA_PERC = W_AREA_PERC*100)
results_habitats_dates <- read.csv2("C:/Users/jonas.gaigr/N2K.CZ/results/results_habitats_20220927.csv") %>%
  dplyr::select(SITECODE, HABITAT_CODE, DATE_MIN, DATE_MAX, DATE_MEDIAN, DATE_MEAN)
results_habitats_read <- results_habitats_read %>%
  dplyr::left_join(., results_habitats_dates, by = c("SITECODE", "HABITAT_CODE"))
paseky_read <- read.csv2("C:/Users/jonas.gaigr/N2K.CZ/results/paseky_results_20220927.csv")
results_habitats_read <- results_habitats_read %>%
  dplyr::left_join(.,
                   paseky_read,
                   by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::mutate(ROZLOHA_KOMPLET = dplyr::case_when(is.na(ROZLOHA_PASEKY) == FALSE ~ ROZLOHA + ROZLOHA_PASEKY,
                                                   TRUE ~ ROZLOHA)) %>%
  dplyr::relocate(ROZLOHA_KOMPLET, .after = ROZLOHA)


# NASTAVENÍ LIMITNÍCH HODNOT ----
limits <- matrix(as.integer(NA), nrow(sites_habitats), ncol(hablimits)+2) %>% 
  dplyr::as_tibble() 
colnames(limits) <- c("SITECODE", "NAZEV", colnames(hablimits))
limits <- limits %>%
  mutate(SITECODE = as.character(SITECODE),
         NAZEV = as.character(NAZEV),
         HABITAT_CODE = as.character(HABITAT_CODE),
         TD_LIM = as.integer(TD_LIM),
         QUAL_LIM = as.integer(QUAL_LIM),
         MINIMIAREAL_LIM = as.integer(MINIMIAREAL_LIM),
         MOZAIKA_LIM = as.integer(MOZAIKA_LIM),
         CELISTVOST_LIM = as.integer(CELISTVOST_LIM),
         KONEKTIVITA_LIM = as.integer(KONEKTIVITA_LIM),
         REDLIST_LIM = as.logical(REDLIST_LIM),
         INVASIVE_LIM = as.integer(INVASIVE_LIM),
         EXPANSIVE_LIM = as.integer(INVASIVE_LIM),
         CONFLICT = as.integer(CONFLICT),
         REG = as.character(REG))

for(i in 1:nrow(sites_habitats)) {
  limits[i,1] <- sites_habitats[i,1]
  limits[i,2] <- sites_habitats[i,2]
  limits[i,3:14] <- (find_habitat_LIMIT(sites_habitats[i,5]))
}

# HODNOTY PARAMETRŮ VZTAŽENÉ K LIMITNÍM HODNOTÁM ----
results_habitats_limits <- left_join(results_habitats_read, limits)
results_habitats_values <- results_habitats_limits %>%
  mutate(TD_DIF = TYPICKE_DRUHY - TD_LIM,
         QUAL_DIF_ORIG = KVALITA_ORIG - QUAL_LIM,
         QUAL_DIF = KVALITA - QUAL_LIM,
         MINIMIAREAL_DIF = MINIMIAREAL - MINIMIAREAL_LIM,
         MOZAIKA_DIF = MOZAIKA - MOZAIKA_LIM,
         CELISTVOST_DIF = CELISTVOST - CELISTVOST_LIM,
         KONEKTIVITA_DIF = KONEKTIVITA - KONEKTIVITA_LIM,
         INVASIVE_DIF = INVASIVE - INVASIVE_LIM,
         EXPANSIVE_DIF = EXPANSIVE - INVASIVE_LIM) %>%
  dplyr::mutate(TD_ABSL = case_when(TD_DIF < 0 ~ 0,
                                    is.na(TD_DIF) ~ 0,
                                    TD_DIF >= 0 ~ 1),
                QUAL_ABSL = case_when(QUAL_DIF < 0 ~ 0,
                                      is.na(QUAL_DIF) ~ 0,
                                      QUAL_DIF >= 0 ~ 1),
                QUAL_ABSL_ORIG = case_when(QUAL_DIF_ORIG < 0 ~ 0,
                                           is.na(QUAL_DIF_ORIG) ~ 0,
                                           QUAL_DIF_ORIG >= 0 ~ 1),
                MINIMIAREAL_ABSL = case_when(MINIMIAREAL_DIF < 0 ~ 0,
                                             is.na(MINIMIAREAL_DIF) ~ 0,
                                             MINIMIAREAL_DIF >= 0 ~ 1),
                MOZAIKA_ABSL = case_when(MOZAIKA_DIF < 0 ~ 0,
                                         is.na(MOZAIKA_DIF) ~ 0,
                                         MOZAIKA_DIF >= 0 ~ 1),
                CELISTVOST_ABSL = case_when(CELISTVOST_DIF < 0 ~ 0,
                                            is.na(CELISTVOST_DIF) ~ 0,
                                            CELISTVOST_DIF >= 0 ~ 1),
                KONEKTIVITA_ABSL = case_when(KONEKTIVITA_DIF < 0 ~ 0,
                                             is.na(KONEKTIVITA_DIF) ~ 0,
                                             KONEKTIVITA_DIF >= 0 ~ 1),
                INVASIVE_ABSL = case_when(INVASIVE_DIF < 0 ~ 0,
                                          is.na(INVASIVE_DIF) ~ 0,
                                          INVASIVE_DIF >= 0 ~ 1),
                EXPANSIVE_ABSL = case_when(EXPANSIVE_DIF < 0 ~ 0,
                                           is.na(EXPANSIVE_DIF) ~ 0,
                                           EXPANSIVE_DIF >= 0 ~ 1)) %>%
  dplyr::group_by(NAZEV, HABITAT_CODE) %>%
  dplyr::mutate(OVERALL_SUM = sum(TD_DIF, QUAL_DIF, MINIMIAREAL_DIF, 
                                  MOZAIKA_DIF, CELISTVOST_DIF, KONEKTIVITA_DIF, 
                                  INVASIVE_DIF, EXPANSIVE_DIF),
                PAR_KLIC = sum(QUAL_ABSL, MINIMIAREAL_ABSL),
                PAR_ALL = case_when(sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) >= 5 &
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) == 2 ~ 1,
                                    sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) >= 4 &
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) >= 1 ~ 0.5,
                                    sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) <= 3 |
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) == 0 ~ 0),
                OBJ_AREA = dplyr::case_when(MINIMIAREAL_DIF < 0 ~ "Enlarge the area",
                                            TRUE ~ NA_character_),
                OBJ_IMPROVE = dplyr::case_when(TD_DIF < 0 |
                                                 QUAL_DIF < 0 ~ "Improve the habitats condition",
                                               TRUE ~ NA_character_),
                OBJ_PRESENCE = dplyr::case_when(ROZLOHA == 0 & 
                                                  HABITAT_CODE != 8310 ~ "Re-establish the habitat",
                                                TRUE ~ NA_character_),
                OBJ_OTHER = dplyr::case_when(MOZAIKA_DIF < 0 | 
                                               CELISTVOST_DIF < 0 | 
                                               KONEKTIVITA_DIF < 0 |
                                               INVASIVE_DIF < 0 | 
                                               EXPANSIVE_DIF < 0 ~ "Other",
                                             TRUE ~ NA_character_),
                OBJ_LIST = dplyr::case_when(is.na(OBJ_PRESENCE) == TRUE ~ toString(na.omit(c(OBJ_AREA,
                                                                                             OBJ_IMPROVE,
                                                                                             OBJ_OTHER))),
                                            TRUE ~ toString(OBJ_PRESENCE))) %>%
  ungroup() %>%
  dplyr::rename(REPRE_RB = REPRE,
                PERC_0_VLNA = PERC_0,
                PERC_1_VLNA = PERC_1,
                PERC_2_VLNA = PERC_2)

write.csv(results_habitats_values,
          "C:/Users/jonas.gaigr/N2K.CZ/results/habitaty_vyhodnoceni_20221021_UTF-8.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_habitats_values, 
           "C:/Users/jonas.gaigr/N2K.CZ/results/habitaty_vyhodnoceni_20221021_windows1250.csv", 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# DATATABLE ----
# VÝSLEDKY HODNOCENÍ S BAREVNĚ VYZNAČENÝMI HODNOTAMI PRARAMETRŮ VE VZTAHU K LIMITNÍM HODNOTÁM
library(DT)
# KRÁTKÁ TABULKA PRO JEDNODUCHOU PREZENTACI
habitats_datatable_sum <- DT::datatable(results_habitats_values %>%
                                          dplyr::select(-c(TD_LIM,
                                                           QUAL_LIM,
                                                           MINIMIAREAL_LIM,
                                                           MOZAIKA_LIM,
                                                           CELISTVOST_LIM,
                                                           KONEKTIVITA_LIM,
                                                           INVASIVE_LIM,
                                                           EXPANSIVE_LIM,
                                                           TD_ABSL,
                                                           QUAL_ABSL,
                                                           MINIMIAREAL_ABSL,
                                                           MOZAIKA_ABSL,
                                                           CELISTVOST_ABSL,
                                                           KONEKTIVITA_ABSL,
                                                           INVASIVE_ABSL,  
                                                           EXPANSIVE_ABSL)) %>%
                                          dplyr::mutate(across(c(ROZLOHA,
                                                                 TYPICKE_DRUHY,
                                                                 KVALITA,
                                                                 MINIMIAREAL,
                                                                 MOZAIKA,
                                                                 CELISTVOST,
                                                                 KONEKTIVITA,
                                                                 RED_LIST,
                                                                 INVASIVE,
                                                                 EXPANSIVE,
                                                                 VYPLNENOST_TD,
                                                                 VYPLNENOST_KVALITA,
                                                                 OVERALL_SUM),
                                                               round, 3)) %>%
                                          as.data.frame(),
                                        extensions = 'Buttons',
                                        options = list(
                                          autowidth = TRUE,
                                          dom = 'Bfrtip',
                                          columnDefs = list(list(targets = 18:28, visible = FALSE)),
                                          buttons = c('csv', 'excel'),
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$('body').css({'font-family': 'Calibri'});",
                                            "}"
                                          )),
                                        rownames = FALSE,
                                        filter = "top") %>%
  DT::formatStyle('SITECODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_CODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('TYPICKE_DRUHY', 'TD_DIF', 
                  backgroundColor = styleInterval(0, 
                                                  c('red', 'green'))) %>%
  DT::formatStyle('KVALITA', 'QUAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MINIMIAREAL', 'MINIMIAREAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MOZAIKA', 'MOZAIKA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('CELISTVOST', 'CELISTVOST_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('KONEKTIVITA', 'KONEKTIVITA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('INVASIVE', 'INVASIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('EXPANSIVE', 'EXPANSIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('PAR_ALL',
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) 
habitats_datatable_sum

# KOMPLETNÍ VÝSLEDKY VČETNĚ LIMITNÍCH HODNOT
habitats_datatable_all <- DT::datatable(results_habitats_values %>%
                                          dplyr::select(-c(TD_ABSL,
                                                           QUAL_ABSL,
                                                           MINIMIAREAL_ABSL,
                                                           MOZAIKA_ABSL,
                                                           CELISTVOST_ABSL,
                                                           KONEKTIVITA_ABSL,
                                                           INVASIVE_ABSL,  
                                                           EXPANSIVE_ABSL)) %>%
                                          mutate(across(c(ROZLOHA,
                                                          TYPICKE_DRUHY,
                                                          KVALITA,
                                                          MINIMIAREAL,
                                                          MOZAIKA,
                                                          CELISTVOST,
                                                          KONEKTIVITA,
                                                          RED_LIST,
                                                          INVASIVE,
                                                          EXPANSIVE,
                                                          TD_LIM,
                                                          QUAL_LIM,
                                                          MINIMIAREAL_LIM,
                                                          MOZAIKA_LIM,
                                                          CELISTVOST_LIM,
                                                          KONEKTIVITA_LIM,
                                                          INVASIVE_LIM,
                                                          EXPANSIVE_LIM,
                                                          VYPLNENOST_TD,
                                                          VYPLNENOST_KVALITA,
                                                          OVERALL_SUM), round, 3)) %>%
                                          as.data.frame(),
                                        extensions = 'Buttons',
                                        options = list(
                                          dom = 'Bfrtip',
                                          buttons = c('csv', 'excel'),
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$('body').css({'font-family': 'Calibri'});",
                                            "}"
                                          )),
                                        rownames = FALSE,
                                        filter = "top") %>%
  DT::formatStyle('SITECODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_CODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('TYPICKE_DRUHY', 'TD_DIF', 
                  backgroundColor = styleInterval(0, 
                                                  c('red', 'green'))) %>%
  DT::formatStyle('KVALITA', 'QUAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MINIMIAREAL', 'MINIMIAREAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MOZAIKA', 'MOZAIKA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('CELISTVOST', 'CELISTVOST_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('KONEKTIVITA', 'KONEKTIVITA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('INVASIVE', 'INVASIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('EXPANSIVE', 'EXPANSIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('PAR_ALL',
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green")))
habitats_datatable_all

# EXPORT GRAFŮ ----
results_habitats_values_plot <- as.data.frame(results_habitats_values)
results_habitats_values_plot[is.na(results_habitats_values_plot)] <- 0

for(i in 1:nrow(sites_habitats)) {
  file_name_prep <- paste(results_habitats_read[i,2], results_habitats_read[i,3], sep = "_")
  file_name <- paste("S:/Gaigr/hodnoceni_stanovist_grafy/", "hodnoceni_", file_name_prep, ".png", sep = "")
  result <- hvezdice_plot(as.data.frame(results_habitats_read[i,]))
  ggplot2::ggsave(result, filename = file_name, height = 8, width = 12, units = "in")
}

results_habitats_values_plot %>% 
  dplyr::select(TD_LIM,
                QUAL_LIM,
                MINIMIAREAL_LIM,
                MOZAIKA_LIM,
                CELISTVOST_LIM,
                KONEKTIVITA_LIM,
                INVASIVE_LIM,
                EXPANSIVE_LIM,
                CONFLICT)
results_habitats$SITECODE %in% SDO_sites$Kód.lokality 

# SDO II LINK ----
SDO_sites <- read.csv2("C:/Users/jonas.gaigr/N2K.CZ/SDO_II_predmetolokality.csv")
results_habitats_values_SDO <- results_habitats_values %>%
  dplyr::mutate(SDO_II = case_when(SITECODE %in% SDO_sites$sitecode ~ 1,
                            TRUE ~ 0))
results_habitats_values_SDO <- results_habitats_values_SDO %>%
  dplyr::filter(SDO_II == 1)

write.csv(results_habitats_values_SDO, 
          "C:/Users/jonas.gaigr/N2K.CZ/results/habitaty_vyhodnoceni_202209029_SDOII_encoded.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv2(results_habitats_values_SDO, 
           "C:/Users/jonas.gaigr/N2K.CZ/results/habitaty_vyhodnoceni_202209029_SDOII_windows.csv", 
           row.names = FALSE,
           fileEncoding = "Windows-1250")
