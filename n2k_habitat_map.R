# Packages ----
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(DT)
library(zoo)
library(lubridate)
library(condformat)
library(ggplot2)
library(rgdal)
library(raster)
library(ggsn)
library(proj4)
library(sp)
library(sf)
library(leaflet)
library(openxlsx)
library(fmsb)
options(shiny.sanitize.errors = FALSE)

# LOAD DATA ----
# VRSTVA EVL 
evl <- st_read("//bali.nature.cz/du/OchranaPrirody/Natura 2000/EvVyzLok_440_2021.shp")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))
# VRSTVA HRANIC CZ 
czechia <- st_read("//bali.nature.cz/du/SpravniCleneni/CR/HraniceCR.shp")
czechia <- st_transform(czechia, CRS("+init=epsg:4326"))
# VMB
vmb_shp_sjtsk_22_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/20220531_Segment.shp")
vmb_hab_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/HAB_BIOTOP.dbf")
vmb_pb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/PB_BIOTOP.dbf") %>%
  dplyr::filter(!OBJECTID %in% vmb_hab_dbf_22$OBJECTID)
vmb_hab_pb_dbf_22 <- dplyr::bind_rows(vmb_hab_dbf_22, vmb_pb_dbf_22) %>%
  dplyr::group_by(SEGMENT_ID) %>%
  dplyr::mutate(moz_num = n(),
                FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                 sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                   sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                 sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(SEGMENT_ID,
                FSB_EVAL_prep) %>%
  distinct()

vmb_shp_sjtsk_22 <- vmb_shp_sjtsk_22_read %>%
  dplyr::left_join(vmb_hab_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::left_join(vmb_hab_pb_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "moz." ~ "moz.",
                                            FSB_EVAL_prep == "X" ~ "X",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP %in% c("T3.3C", "3.4A", "T3.4C", "T3.5A") ~ "6210p",
                                           TRUE ~ HABITAT))
# SEZNAM PŘEDMĚTOLOKALIT
sites_subjects <- openxlsx::read.xlsx("https://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000_440_2021.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")
sites_habitats <- sites_subjects %>%
  filter(Typ.předmětu.ochrany == "stanoviště")

# ČÍSELNÍK HABITATŮ
habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
minimisize <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/minimisize.csv", encoding = "UTF-8")
# REDLISTOVÉ, INVAZNÍ A EXPANZNÍ DRUHY
evl_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_cev_rostliny_EVL_09122022.xlsx")
# STAŽENÍ PŘES GDRIVE "https://docs.google.com/spreadsheets/d/12n1eCbkw8ufsFoUnKvEMMg123bfMFOzI/edit?usp=sharing&ouid=102654749342263703541&rtpof=true&sd=true"
evl_expansive_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_expanzivky_EVL_09122022.xlsx")

# EXPANZNÍ DRUHY
expansive_species <- evl_expansive_species %>%
  dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
  dplyr::mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
                DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
                NEGATIVNI = dplyr::case_when(NEGATIVNI == "ne" ~ 0,
                                             NEGATIVNI == "ano" ~ 1))%>%
  sf::st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# DRUHY ČERVENÉHO SEZNAMU S ODFILTROVANÝMI C4 DRUHY
red_list_species <- evl_species %>%
  dplyr::filter(is.na(Nepůvodní.druhy) == TRUE) %>%
  dplyr::filter(is.na(Druhy.červeného.seznamu) == FALSE) %>%
  dplyr::mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
                DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  sf::st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# INVAZNÍ DRUHY
invasive_species <- evl_species %>%
  dplyr::filter(is.na(Nepůvodní.druhy) == FALSE) %>%
  dplyr::filter(Nepůvodní.druhy != "GL - šedý seznam: výskyt tolerován") %>%
  dplyr::mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
                DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
                NEGATIVNI = dplyr::case_when(NEGATIVNI == "ne" ~ 0,
                                             NEGATIVNI == "ano" ~ 1)) %>%
  sf::st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")



find.evl.SITECODE <- function(species) {
  return(subset(sites_subjects, sites_subjects$Název.česky == species)$Kód.lokality )
}
find.evl.NAZEV <- function(species) {
  return(subset(sites_subjects, sites_subjects$Název.česky == species)$Název.lokality)
}
find.evl.NUMBER <- function(species) {
  return(nrow(subset(sites_subjects, sites_subjects$Název.česky == species)))
}
find.habitat.NAME_CZ <-function(species) {
  return(subset(habitats, habitats$HABITAT_CODE == species)$HABITAT_CZ)
}
find.habitat.CODE <-function(species) {
  return(subset(habitats, habitats$HABITAT_CZ == species)$HABITAT_CODE)
}
find.evl.MINIMISIZE <- function(species) {
  return(mean(subset(minimisize, minimisize$HABITAT == species)$MINIMISIZE))
}
find.habitat.INVADERS <- function(species) {
  return(mean(subset(minimisize, minimisize$HABITAT == species)$MINIMISIZE))
}
find_evl_NAME_TO_CODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.lokality == species) %>%
           dplyr::pull(Kód.lokality) %>%
           unique()
  )
}

# UI ----
ui <- fluidPage(
  theme = "bootstrap.css",
  
  tags$head(tags$link(rel = "shortcut icon", 
                      href = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/favicon.ico")),
  
  titlePanel(div(HTML("<h1>Hodnocení stavu evropsky významných stanovišť")
  ),
  windowTitle = "Hodnocení stavu evropsky významných stanovišť"),
  
  br(),
  
  column(2,
         htmlOutput("group_selector")),
  column(4,
         htmlOutput("habitat_selector")),
  column(5,
         htmlOutput("evl_selector")),
  
  br(),
  br(),
  br(),
  br(),
  br(),
  
  hr(),
  
  fluidRow(
    uiOutput("default")
  ),
  
  fluidRow(
    column(12, 
           leafletOutput(outputId = "mapa_evl", height = "600px", width = "100%") %>%
             withSpinner(color = "green")
    ),

  ),

  br(),
  br(),
  
  hr(),
  
  #hr(),
  
  fluidRow(div(img(
    src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg", 
    height = 70), style = "text-align: center;")),
  
  br(),
  
  fluidRow(HTML('<center><b>© 2021-2023 <a href="http://www.nature.cz" target="_blank">AOPK ČR</a></b></center>')),
  br()
)

# SERVER ----
server <- function(input, output, session) {
  
  # Stanovení maximální velikosti nahrávaného souboru
  options(shiny.maxRequestSize=50*1024^2)
  
  # MAPA EVL ----
  output$mapa_evl <- renderLeaflet({
    
    req(input$evl_site)
    
    input_habitat_cz <- input$habitat_cz
    input_evl_site <- input$evl_site
    
    hvezdice_spat <- function(hab_code, evl_site) {
      
      # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
      vmb_target_sjtsk <- vmb_shp_sjtsk_22 %>%
        sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
        dplyr::filter(HABITAT == hab_code) %>%
        dplyr::mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
        dplyr::mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA)
      
      result <- vmb_target_sjtsk
      
      result
    }
    
    target_evl <- filter(evl_sjtsk, SITECODE == find_evl_NAME_TO_CODE(input_evl_site))
    
    map_habitat <- hvezdice_spat(find.habitat.CODE(input_habitat_cz), 
                                 find_evl_NAME_TO_CODE(input_evl_site))
    
    if(find.habitat.CODE(input_habitat_cz) == 6510) {
      map_invaders_point <- invasive_species %>%
        dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
        sf::st_intersection(., map_habitat) %>%
        dplyr::filter(is.na(HABITAT) == FALSE) %>%
        dplyr::group_by(OBJECTID.y, DRUH) %>%
        dplyr::filter(DATUM_OD >= DATUM.x) %>%
        dplyr::slice(which.max(DATUM_OD)) %>%
        dplyr::filter(NEGATIVNI == 0) %>%
        dplyr::ungroup()
    } else {
      map_invaders_point <- invasive_species %>%
        sf::st_intersection(., map_habitat) %>%
        dplyr::filter(is.na(HABITAT) == FALSE) %>%
        dplyr::group_by(OBJECTID.y, DRUH) %>%
        dplyr::filter(DATUM_OD >= DATUM.x) %>%
        dplyr::slice(which.max(DATUM_OD)) %>%
        dplyr::filter(NEGATIVNI == 0) %>%
        dplyr::ungroup()
    }
    
    map_invaders_segment <- map_habitat %>%
      dplyr::filter(SEGMENT_ID %in% map_invaders_point$SEGMENT_ID)
    
    map_expanders <- expansive_species %>%
      dplyr::filter(POKRYVNOST %in% c("3", "4", "5")) %>%
      sf::st_intersection(., map_habitat) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
    
   if(nrow(map_habitat) != 0 & nrow(map_invaders_point) != 0 & nrow(map_expanders) != 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = st_transform(target_evl, CRS("+init=epsg:4326")),
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y,
                   group = "EVL") %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addPolygons(data = st_transform(map_invaders_segment, CRS("+init=epsg:4326")),
                   color = "red",
                   fill = "red",
                   weight = 3,
                   opacity = 1,
                   group = "Invadované polygony") %>%
       addCircles(data = st_transform(map_invaders_point, CRS("+init=epsg:4326")),
                  color = "red",
                  fill = "red",
                  weight = 5,
                  opacity = 1,
                  label = ~DRUH,
                  group = "Invazní druhy") %>%
       addCircles(data = st_transform(map_expanders, CRS("+init=epsg:4326")),
                  color = "black",
                  fill = "black",
                  weight = 5,
                  opacity = 1,
                  label = ~DRUH,
                  group = "Expanzní druhy") %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        overlayGroups = c("EVL", 
                                          "Invazní druhy", 
                                          "Expanzní druhy", 
                                          "Invadované polygony"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders_point) == 0 & nrow(map_expanders) != 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = st_transform(target_evl, CRS("+init=epsg:4326")),
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y,
                   group = "EVL") %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addCircles(data = st_transform(map_expanders, CRS("+init=epsg:4326")),
                  color = "black",
                  fill = "black",
                  weight = 5,
                  opacity = 1,
                  label = ~DRUH,
                  group = "Expanzní druhy") %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        overlayGroups = c("EVL", 
                                          "Expanzní druhy"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders_point) != 0 & nrow(map_expanders) == 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = st_transform(target_evl, CRS("+init=epsg:4326")),
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y,
                   group = "EVL") %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addPolygons(data = st_transform(map_invaders_segment, CRS("+init=epsg:4326")),
                   color = "red",
                   fill = "red",
                   weight = 3,
                   opacity = 1,
                   group = "Invadované polygony") %>%
       addCircles(data = st_transform(map_invaders_point, CRS("+init=epsg:4326")),
                  color = "red",
                  fill = "red",
                  weight = 5,
                  opacity = 1,
                  label = ~DRUH,
                  group = "Invazní druhy") %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        overlayGroups = c("EVL", 
                                          "Invadované polygony",
                                          "Invazní druhy"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders_point) == 0 & nrow(map_expanders) == 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = st_transform(target_evl, CRS("+init=epsg:4326")),
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y,
                   group = "EVL") %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        overlayGroups = c("EVL"),
                        options = layersControlOptions(collapsed = FALSE))
   }
    
  })
  

  # Výběr habitatu a EVL ----
  output$group_selector = renderUI({
    selectInput(inputId = "Group",
                label = "Vyberte skupinu",
                choices = as.character(unique(habitats$GROUP)))
  })
  
  output$habitat_selector = renderUI({
    data_available = habitats[habitats$GROUP == input$Group, "HABITAT_CZ"]
    selectInput(
      inputId = "habitat_cz",
      label = "Vyberte stanoviště",
      choices = unique(data_available),
      selected = unique(data_available)[1]
    )
  })
  
  output$evl_selector = renderUI({
    
    evl_available = find.evl.NAZEV(input$habitat_cz)
    selectInput(
      inputId = "evl_site",
      label = "Vyberte EVL",
      choices = c("", as.character(evl_available)),
      selected = NULL
    )
  })
  
  
  # Lokality ----
  output$lokality <- renderTable({
    
    req(input$species)
    
    cbind("Název EVL" = find.evl.NAZEV(input$habitat_cz), 
          "SITECODE" = find.evl.SITECODE(input$habitat_cz))
    
  })
  
  # Default UI ----
  output$default <- renderUI({
    
    if (input$habitat_cz == "" | input$evl_site == "") {
      
      div(img(src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/logo.png",
              width = "30%"), style = "text-align: center;")
      
    } else {
      NULL
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
