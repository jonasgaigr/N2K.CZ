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

evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))
czechia <- st_read("HraniceCR.shp")
czechia <- st_transform(czechia, CRS("+init=epsg:4326"))
#bioregs <- st_read("BiogeoRegions_CR.shp")
#bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))
vmb_shp_sjtsk <- st_read("20200608_Segment.shp")
vmb_hab_dbf <- st_read("HAB_BIOTOP.dbf")
vmb_shp_sjtsk <- vmb_shp_sjtsk %>%
  left_join(vmb_hab_dbf, by = "SEGMENT_ID") %>%
  mutate(FSB = case_when(STEJ_PR < 75 ~ "X",
                         STEJ_PR >= 75 | STEJ_PR < 100 ~ "moz."))

sites_subjects <- read.xlsx("http://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")

habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
minimisize <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/minimisize.csv", encoding = "UTF-8")
evl_lengths <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_max_dist.csv", encoding = "UTF-8")
#evl_species <- read.csv("https://media.githubusercontent.com/media/jonasgaigr/N2K.CZ/main/cevnate_evl.csv", encoding = "UTF-8")
evl_species <- openxlsx::read.xlsx("https://github.com/jonasgaigr/N2K.CZ/blob/main/cevnate_evl.xlsx?raw=true")
evl_expansive_species <- openxlsx::read.xlsx("export_nalezy_expanzivky_EVL.xlsx")
expansive_species <- evl_expansive_species %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  filter(DRUH != "Arrhenatherum elatius") %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

red_list_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == TRUE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

invasive_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == FALSE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")



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
  
  fluidRow(
    column(6,
           htmlOutput(outputId = "invaders_list")),
    column(6,
           htmlOutput(outputId = "expanders_list"))
  ),
  
  br(),
  br(),
  
  hr(),
  
  #hr(),
  
  fluidRow(div(img(
    src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg", 
    height = 70), style = "text-align: center;")),
  
  br(),
  
  fluidRow(HTML('<center><b>© 2021-2022 <a href="http://www.nature.cz" target="_blank">AOPK ČR</a></b></center>')),
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
    
    hvezdice.spat <- function(hab_code, evl_site) {
      vmb_qual <- vmb_shp_sjtsk %>%
        st_intersection(filter(evl_sjtsk, NAZEV == evl_site)) %>%
        filter(HABITAT == hab_code) %>%
        mutate(TYP_DRUHY_SEG = case_when(TD == "N" ~ 0,
                                         TD == "MP" ~ 5,
                                         TD == "P" ~ 10),
               TD_SEG = TYP_DRUHY_SEG*PLO_BIO_M2/sum(PLO_BIO_M2),
               QUAL = case_when(KVALITA == 1 ~ 1,
                                KVALITA == 2 ~ 1,
                                KVALITA == 3 ~ 2,
                                KVALITA == 4 ~ 2)) %>%
        # TYPICKÉ DRUHY
        mutate(TD_FIN = sum(na.omit(.$TD_SEG))) %>%
        # KVALITA
        mutate(QUALITY = sum(filter(., QUAL == 1)$PLO_BIO_M2)/sum(filter(., QUAL == 1 | QUAL == 2)$PLO_BIO_M2)*10,
               # MINIMIAREÁL
               MINIMIAREAL = sum(filter(., QUAL == 1)$PLO_BIO_M2)/find.evl.MINIMISIZE(hab_code),
               MINIMIAREAL = case_when(MINIMIAREAL > 10 ~ 10, 
                                       MINIMIAREAL <= 10 ~ MINIMIAREAL),
               MINIMISIZE = case_when(HABITAT == hab_code & PLO_BIO_M2 > find.evl.MINIMISIZE(hab_code) ~ 1,
                                      HABITAT == hab_code & PLO_BIO_M2 <= find.evl.MINIMISIZE(hab_code) ~ 0))
      
      spat_multi <- vmb_qual %>%
        filter(lengths(st_touches(geometry)) > 0)
      spat_single <- vmb_qual %>%
        filter(lengths(st_touches(geometry)) == 0)
      spat_union <- spat_single %>%
        bind_rows(spat_multi) %>%
        st_union() %>%
        st_cast(., "POLYGON") %>%
        as.data.frame() %>%
        st_as_sf() %>%
        mutate(SHAPE_AREA = as.numeric(st_area(geometry)),
               MINIMI_SIZE = case_when(SHAPE_AREA > find.evl.MINIMISIZE(hab_code) ~ 1,
                                       SHAPE_AREA <= find.evl.MINIMISIZE(hab_code) ~ 0)) %>%
        mutate(CELISTVOST = sum(filter(., MINIMI_SIZE == 1)$SHAPE_AREA)/sum(SHAPE_AREA)*10) %>%
        arrange(-MINIMI_SIZE)
      
      
      result <- spat_union %>%
        summarise(SITECODE = unique(vmb_qual$SITECODE),
                  NAZEV = unique(vmb_qual$NAZEV),
                  HABITAT_CODE = unique(vmb_qual$HABITAT))
      
      result
    }
    
    target_evl <- filter(evl, NAZEV == input_evl_site)
    
    map_habitat <- hvezdice.spat(find.habitat.CODE(input_habitat_cz), input_evl_site)
    
    map_invaders <- invasive_species %>%
      sf::st_intersection(., map_habitat) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.y) %>%
      dplyr::slice(which.max(DATUM.y)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
    
    map_expanders <- expansive_species %>%
      st_filter(., map_habitat)
    
   if (nrow(map_habitat) != 0 & nrow(map_invaders) != 0 & nrow(map_expanders) != 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = target_evl,
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y) %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addCircles(data = st_transform(map_invaders, CRS("+init=epsg:4326")),
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
                        overlayGroups = c("Invazní druhy", "Expanzní druhy"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders) == 0 & nrow(map_expanders) != 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = target_evl,
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y) %>%
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
                        overlayGroups = c("Expanzní druhy"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders) != 0 & nrow(map_expanders) == 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = target_evl,
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y) %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addCircles(data = st_transform(map_invaders, CRS("+init=epsg:4326")),
                  color = "red",
                  fill = "red",
                  weight = 5,
                  opacity = 1,
                  label = ~DRUH,
                  group = "Invazní druhy") %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        overlayGroups = c("Invazní druhy"),
                        options = layersControlOptions(collapsed = FALSE))
   } else if (nrow(map_habitat) != 0 & nrow(map_invaders) == 0 & nrow(map_expanders) == 0) {
     leaflet() %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
       addPolygons(data = target_evl,
                   color = "#65c3d5",
                   fill = "#65c3d5",
                   weight = 3,
                   opacity = .97,
                   label = target_evl$NAZEV.y) %>%
       addPolygons(data = st_transform(map_habitat, CRS("+init=epsg:4326")),
                   color = "#a3c935",
                   fill = "#a3c935",
                   weight = 3,
                   opacity = 1) %>%
       addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                        options = layersControlOptions(collapsed = FALSE))
   }
    
  })
  
  # SEZNAM INVAZNÍCH A EXPANZIVNÍCH TAXONŮ ----
  output$taxalist <- renderDataTable({
    
    req(input$evl_site, input$habitat_cz)
    input_habitat_cz <- input$habitat_cz
    input_evl_site <- input$evl_site
    current_year <- input$curryear
    
    hvezdice.list <- function(hab_code, evl_site) {
      
      vmb_target_sjtsk <- vmb_shp_sjtsk %>%
        st_intersection(filter(evl_sjtsk, NAZEV == evl_site)) %>%
        filter(HABITAT == hab_code)
      
      invaders_list <- invasive_species %>%
        st_filter(., vmb_target_sjtsk) %>%
        select(DRUH) %>%
        mutate(SKUPINA == "INVAZIVNÍ")
      
      expanders_list <- expansive_species %>%
        st_filter(., vmb_target_sjtsk) %>%
        select(DRUH) %>%
        mutate(SKUPINA == "EXPANZIVNÍ")
      
      result_list <- bind_rows(expansive_species, invasive_species) %>%
        arrange(DRUH)
      
      result_list
    }
    
    hvezdice.list(find.habitat.CODE(input_habitat_cz), input_evl_site)
    
  })
  
  # SEZNAM RED LIST TAXONŮ ----
  output$taxalist <- renderDataTable({
    
    req(input$evl_site, input$habitat_cz)
    input_habitat_cz <- input$habitat_cz
    input_evl_site <- input$evl_site
    current_year <- input$curryear
    
    hvezdice.list <- function(hab_code, evl_site) {
      
      vmb_target_sjtsk <- vmb_shp_sjtsk %>%
        st_intersection(filter(evl_sjtsk, NAZEV == evl_site)) %>%
        filter(HABITAT == hab_code)
      
      red_list <- red_list_species %>%
        st_filter(., vmb_target_sjtsk) %>%
        select(DRUH, )
      
      result_list
    }
    
    hvezdice.list(find.habitat.CODE(input_habitat_cz), input_evl_site)
    
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
