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
#evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl <- st_read("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/Evropsky_v%C3%BDznamn%C3%A9_lokality.geojson")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))
# HRANICE ČR
czechia <- st_read("HraniceCR.shp")
czechia_line <- st_cast(czechia, "LINESTRING")
# BIOGEOGRAFICKÉ ČLENĚNÍ ČR
#bioregs <- st_read("BiogeoRegions_CR.shp")
#bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))
# VMB - MÁJOVÁ VRSTVA 2020
vmb_shp_sjtsk <- st_read("20200608_Segment.shp")
vmb_hab_dbf <- st_read("HAB_BIOTOP.dbf")
vmb_shp_sjtsk <- vmb_shp_sjtsk %>%
  left_join(vmb_hab_dbf, by = "SEGMENT_ID") %>%
  # METODIKA KUČERY ET AL. VYŘAZUJE Z ALGORITMU SEGMENTY SE ZESTOUPENÍM PŘÍRODNÍCH
  # BIOTOPŮ < 75 % - NECHÁVÁM K DALŠÍ DISKUZI, ALE HLEDÁM JINÉ ŘEŠENÍ
  mutate(FSB = case_when(STEJ_PR < 50 ~ "X",
                         STEJ_PR >= 50 & STEJ_PR < 100 ~ "moz.",
                         TRUE ~ FSB),
         HABITAT = case_when(BIOTOP == "T3.3C" | 
                               BIOTOP == "3.4A" | 
                               BIOTOP == "T3.4C" | 
                               BIOTOP == "T3.5A" ~ "6210p",
                             BIOTOP != "T3.3C" & 
                               BIOTOP != "3.4A" & 
                               BIOTOP != "T3.4C" & 
                               BIOTOP != "T3.5A" ~ HABITAT))
# SEZNAM EVL A PŘEDMĚTŮ OCHRANY
sites_subjects <- read.xlsx("http://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")
sites_habitats <- sites_subjects %>%
  filter(Typ.předmětu.ochrany == "stanoviště")
# ČÍSELNÍK HABITATŮ
habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
# SEZNAM MINIMIAREÁLŮ
minimisize <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/minimisize.csv", encoding = "UTF-8")
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
  return(mean(subset(minimisize, minimisize$HABITAT == species)$MINIMISIZE))
  return(minimisize %>%
           dplyr::filter(HABITAT == species) %>%
           pull(MINIMIZIZE) %>%
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
  vmb_target_sjtsk <- vmb_shp_sjtsk %>%
    st_intersection(filter(evl_sjtsk, SITECODE == evl_site)) %>%
    filter(HABITAT == hab_code) %>%
    mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
    mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA)
  
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk %>%
    st_filter(., evl_sjtsk %>%
                filter(., SITECODE == evl_site) %>%
                st_buffer(., 500)) %>%
    filter(FSB != "X" | 
             FSB != "-" |
             HABITAT != hab_code) %>% 
    group_by(SEGMENT_ID) %>% 
    slice(1) %>%
    ungroup()
  
  vmb_qual <- vmb_target_sjtsk %>%
    filter(FSB != "X" | is.na(FSB) == FALSE) %>%
    mutate(TYP_DRUHY_SEG = case_when(TD == "N" ~ 0, # PŘEPOČET TYPICKÝCH DRUHŮ NA SEGMENTU
                                     TD == "MP" ~ 5,
                                     TD == "P" ~ 10),
           TD_SEG = TYP_DRUHY_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL),
           QUAL = case_when(KVALITA == 1 ~ 1,
                            KVALITA == 2 ~ 1,
                            KVALITA == 3 ~ 2,
                            KVALITA == 4 ~ 2),
           KVALITA_SEG = case_when(KVALITA == 1 ~ 10
                                   KVALITA == 2 ~ 6.6666666666666666666666,
                                   KVALITA == 3 ~ 3.3333333333333333333333,
                                   KVALITA == 4 ~ 0),
           QUAL_SEG = KVALITA_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL)) %>%
    
    mutate(
      # TYPICKÉ DRUHY
      TD_FIN = sum(na.omit(.$TD_SEG)),
      # KVALITA
      QUALITY = sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/sum(filter(., QUAL == 1 | QUAL == 2)$PLO_BIO_M2_EVL)*10,
      QUALITY = sum(na.omit(QUAL_SEG)),
    # MINIMIAREÁL
      MINIMIAREAL = case_when(find_evl_PRIORITY(hab_code) == 0 ~ sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/find_habitat_MINIMISIZE(hab_code),
                              find_evl_PRIORITY(hab_code) == 1 ~ (sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/find_habitat_MINIMISIZE(hab_code)) + (sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/find_habitat_MINIMISIZE(hab_code)*0.2)),
      MINIMIAREAL = case_when(MINIMIAREAL > 10 ~ 10, 
                              MINIMIAREAL <= 10 ~ MINIMIAREAL),
      MINIMISIZE = case_when(HABITAT == hab_code & PLO_BIO_M2_EVL > find_habitat_MINIMISIZE(hab_code) ~ 1,
                             HABITAT == hab_code & PLO_BIO_M2_EVL <= find_habitat_MINIMISIZE(hab_code) ~ 0))
  
  # PŘÍPRAVA SLOUČENÝH SEGmENTŮ PRO CELISTVOST
  if(find_evl_PRIORITY(hab_code) == 1) {
    spat_multi <- vmb_qual %>%
      filter(QUAL == 1 | QUAL == 2) %>%
      filter(lengths(st_touches(geometry)) > 0)
    spat_single <- vmb_qual %>%
      filter(QUAL == 1 | QUAL == 2) %>%
      filter(lengths(st_touches(geometry)) == 0)
    } else {
    spat_multi <- vmb_qual %>%
      filter(QUAL == 1) %>%
      filter(lengths(st_touches(geometry)) > 0)
    spat_single <- vmb_qual %>%
      filter(QUAL == 1) %>%
      filter(lengths(st_touches(geometry)) == 0)
    }
  
  # SLOUČENÁ VRSTVA POLYGONŮ
  spat_union <- spat_single %>%
    bind_rows(spat_multi) %>%
    st_union() %>%
    st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    st_as_sf() %>%
    mutate(ID_COMB = row_number())
  
  # VÝPOČET PARAMETRU CELISTVOST
  spat_celistvost <- spat_union %>%
    st_intersection(., vmb_qual) %>%
    group_by(ID_COMB) %>%
    mutate(COMB_SIZE = case_when(find_evl_PRIORITY(hab_code) == 0 ~ sum(PLO_BIO_M2_EVL),
                                 find_evl_PRIORITY(hab_code) == 1 ~ (sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/find_habitat_MINIMISIZE(hab_code)) + 
                                   (sum(filter(., QUAL == 1)$PLO_BIO_M2_EVL)/find_habitat_MINIMISIZE(hab_code)*0.2))) %>%
    mutate(MINIMI_SIZE = case_when(COMB_SIZE > find_habitat_MINIMISIZE(hab_code) ~ 1,
                                   COMB_SIZE <= find_habitat_MINIMISIZE(hab_code) ~ 0))
    
  
  if(nrow(filter(vmb_qual, QUAL == 1)) > 0) {
    celistvost <- sum(filter(spat_celistvost, MINIMI_SIZE == 1)$PLO_BIO_M2_EVL)/sum(spat_celistvost$PLO_BIO_M2_EVL)*10
  } else {
    celistvost <- NA
  }
  
  # MAXIMÁLNÍ VZDÁLENOST MEZI 2 BODY V EVL 
  evl_length <- filter(evl_lengths, SITECODE == evl_site) %>% 
    pull(MAX_DIST)
  
  # VÝPOČET PARAMETRU KONEKTIVITA
  if(nrow(filter(vmb_qual, QUAL == 1)) > 0) {
    dist_matrix <-  st_distance(spat_union)
    diag(dist_matrix) <- 1000000000
    dist_matrix <- units::drop_units(dist_matrix)
    connectivity <- case_when(min(dist_matrix) == Inf ~ 0,
                              min(dist_matrix) == 1000000000 ~ 10,
                              min(dist_matrix) < 1000000000 ~ (evl_length - mean(matrixStats::rowMins(dist_matrix)))*10/evl_length)
  } else {
    connectivity <- NA
  }
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  border_nat <- st_intersection(vmb_target_sjtsk, vmb_buff) %>% 
    st_length() %>% 
    units::drop_units() %>%
    sum()
  
  # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
  border_all <- vmb_target_sjtsk %>% 
    st_transform(., CRS("+init=epsg:4326")) %>% 
    st_length() %>% 
    units::drop_units() %>%
    sum()
  
  # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
  border_hsl <- st_intersection(vmb_target_sjtsk, czechia_line) %>% 
    st_length() %>% 
    units::drop_units() %>%
    sum()
  
  # VÝPOČET PARAMETRU MOZAIKA
  mozaika <- border_nat/(border_all-border_hsl)*10
  
  if(nrow(filter(vmb_qual, QUAL == 1)) == 0 |
     nrow(vmb_qual) == 0) {
    mozaika <- NA
  }
  if(mozaika > 10 & is.na(mozaika) == FALSE) {
    mozaika <- 10
  }
  
  # CELKOVÁ ROZLOHA STANOVIŠTĚ V EVL EVL
  target_area_ha <- sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)/10000
  
  if(nrow(vmb_target_sjtsk) == 0) {
    target_area_ha <- 0
  }
  
  # VYPLNĚNOST PARAMETRŮ
  fill_TD <- sum(filter(vmb_target_sjtsk, is.na(TD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_QUAL <- sum(filter(vmb_target_sjtsk, is.na(KVALITA) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  
  # RED LIST SPECIES
  redlist_list <- red_list_species %>%
    sf::st_filter(., vmb_target_sjtsk) %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  redlist <- redlist_list %>%
    length()/log(sum(vmb_target_sjtsk$PLO_BIO_M2_EVL))*4

  if(redlist > 10) {
    redlist <- 10
  } 
  
  if(length(redlist_list) == 0) {
    redlist_list <- NA
  } 
  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.y) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.y) %>%
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

  invaders <- sum(invaders_calc$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)*10
  
  if(length(invaders_list) == 0 |
     nrow(vmb_qual) == 0) {
    invaders_list <- NA
  }
  
  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(OBJECTID.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.y) %>%
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
  
  expanders <- sum(expanders_calc$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)*10
  
  if(length(expanders_list) == 0 |
     nrow(vmb_qual) == 0) {
    expanders_list <- NA
  } 
  
  perc_seg_0 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT == 0) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()
  
  perc_seg_1 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT > 0 & ROK_AKT <= 2012) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()
  
  perc_seg_2 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT > 2012 & ROK_AKT <= 2024) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()
  
  
  # VÝSLEDKY
  if(nrow(vmb_qual) > 0) {
    result <- vmb_qual %>%
      dplyr::summarise(SITECODE = unique(SITECODE),
                       NAZEV = unique(NAZEV),
                       HABITAT_CODE = unique(HABITAT),
                       ROZLOHA = target_area_ha,
                       TYPICKE_DRUHY = unique(TD_FIN),
                       KVALITA = unique(QUALITY),
                       MINIMIAREAL = unique(MINIMIAREAL),
                       MOZAIKA = mozaika,
                       CELISTVOST = celistvost,
                       KONEKTIVITA = connectivity,
                       RED_LIST = redlist,
                       INVASIVE = (10 - invaders),
                       EXPANSIVE = (10 - expanders),
                       RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                       INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                       EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
                       VYPLNENOST_TD = fill_TD,
                       VYPLNENOST_KVALITA = fill_QUAL,
                       PERC_0 = perc_seg_0,
                       PERC_1 = perc_seg_1,
                       PERC_2 = perc_seg_2
                ) %>%
      st_drop_geometry()
  } else {
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = NA,
                     TYPICKE_DRUHY = NA,
                     KVALITA = NA,
                     MINIMIAREAL = NA,
                     MOZAIKA = NA,
                     CELISTVOST = NA,
                     KONEKTIVITA = NA,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     RED_LIST_SPECIES = NA,
                     INVASIVE_LIST = NA,
                     EXPANSIVE_LIST = NA,
                     VYPLNENOST_TD = NA,
                     VYPLNENOST_KVALITA = NA)
  }
  
  result
  
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
hu <- hvezdice_eval(sites_habitats[1,5], sites_habitats[1,2])
habresults_1_900 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1_900) <- colnames(hu)
habresults_901_1893 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_901_1893) <- colnames(hu)

for(i in 1:900) {
  habresults_1_900 <- dplyr::bind_rows(habresults_1_900, as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,2])))
}
for(i in 901:nrow(sites_habitats)) {
  habresults_901_1893 <- dplyr::bind_rows(habresults_901_1893, as.data.frame(hvezdice_eval(sites_habitats[i,5], sites_habitats[i,2])))
}

#write.csv2(habresults_1_900, "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_1.csv", row.names = FALSE)
#write.csv2(habresults_901_1893, "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_2.csv", row.names = FALSE)
results_habitats <- bind_rows(habresults_1_900[c(2:901),], habresults_901_1893[c(2:994),])
write.csv2(results_habitats, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_fin_invexp.csv", 
           row.names = FALSE)
results_habitats_read <-  read.csv2("S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_fin.csv")

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

# HODNOTY PARAMETRŮ VZTAŽENÉ K LIMITNÍM HODNOTÁM
results_habitats_limits <- left_join(results_habitats_read, limits)
results_habitats_values <- results_habitats_limits %>%
  mutate(TD_DIF = TYPICKE_DRUHY - TD_LIM,
         QUAL_DIF = KVALITA - QUAL_LIM,
         MINIMIAREAL_DIF = MINIMIAREAL - MINIMIAREAL_LIM,
         MOZAIKA_DIF = MOZAIKA - MOZAIKA_LIM,
         CELISTVOST_DIF = CELISTVOST - CELISTVOST_LIM,
         KONEKTIVITA_DIF = KONEKTIVITA - KONEKTIVITA_LIM,
         INVASIVE_DIF = INVASIVE - INVASIVE_LIM,
         EXPANSIVE_DIF = EXPANSIVE - INVASIVE_LIM) %>%
  mutate(TD_ABSL = case_when(TD_DIF < 0 ~ 0,
                             is.na(TD_DIF) ~ 0,
                             TD_DIF >= 0 ~ 1),
         QUAL_ABSL = case_when(QUAL_DIF < 0 ~ 0,
                               is.na(QUAL_DIF) ~ 0,
                               QUAL_DIF >= 0 ~ 1),
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
  group_by(NAZEV, HABITAT_CODE) %>%
  mutate(SITECODE = find_evl_NAME_TO_CODE(NAZEV),
         OVERALL_SUM = sum(TD_DIF, QUAL_DIF, MINIMIAREAL_DIF, 
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
                               sum(QUAL_ABSL, MINIMIAREAL_ABSL) == 0 ~ 0)) %>%
  ungroup()

#write.csv2(results_habitats_values, 
#           "S:/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni.csv", 
#           row.names = FALSE)

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
         