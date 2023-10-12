# ---------------------------
# Script name: hugz_leaflet_utils.R
#
# Purpose of script: groups all my utils functions for leaflet maps
#
# Author: Hugo Soubrier
#
# Date Created: 2023-10-12
#
# Copyright (c) Hugo SOUBRIER, 2023
# Email: soubrierhugo@gmail.com

# ---------------------------
# Notes:
#   
#
#
# ---------------------------

# Path functions ----------------------------------------------------------

# fetch shp from the geo database 
get_admin_level <- function(obt_gb_path, country, level) {
  
  require("sf")
  sp_dir <- fs::path(obt_gb_path, country)
  # get latest version directory
  latest <- max(fs::dir_ls(sp_dir, regexp = glue::glue("{country}__"), type = "directory"))
  shp_path <- fs::path(latest, "sf", paste(country, tolower(level), sep = "_"), ext = "rds")
  readr::read_rds(shp_path)
}

#make tooltip template
make_tooltip <- function(df, adm_lvl) {
  ar <- ifelse(is.na(df$attack_rate), "0", round(df$attack_rate, 1))
  glue::glue(
    "<b>{df$name}</b><br>
       Cas: <b>{scales::number(df$cas, accuracy = 1)}</b><br>
       Décès: <b>{df$deces}</b><br>
       Taux d'attaque: <b>{ar}</b><br>"
  ) %>% purrr::map(htmltools::HTML)
}

# make a scaling function to convert real numbers to radii appropriate for leaflet
calc_radius <- function(n, scale_factor = 30) {
  sqrt(n)/sqrt(max(n)) * scale_factor
}


#leaflet basemap 

leaflet_basemap <- function(bbox = NULL, minimap = FALSE){
  
  map <- leaflet::leaflet() %>% 
    
    leaflet::addMapPane(name = "boundaries", zIndex = 300) %>% 
    leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
    leaflet::addMapPane(name = "circles", zIndex = 410) %>% 
    leaflet::addMapPane(name = "place_labels", zIndex = 300) %>% 
    
    leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light")  %>% 
    leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Light", 
                              options = leaflet::leafletOptions(pane = "place_labels")) %>% 
    
    leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>% 
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
    
    leaflet::addScaleBar(position = "bottomright", 
                         options = leaflet::scaleBarOptions(imperial = TRUE)) %>% 
    
    leaflet.extras::addFullscreenControl(position = "topleft") %>% 
    
    leaflet.extras::addResetMapButton() %>% 
    
    leaflet::addLayersControl(baseGroups = c("Light", "OSM", "OSM HOT"), 
                              overlayGroups = c("Group 1", "Group 2"), 
                              position = "topleft") 
  
  if(length(bbox)){
    
    map <- map %>% 
      
      leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) 
  }

  if(minimap){
    
    map <- map %>% 
      
      leaflet::addMiniMap(toggleDisplay = TRUE, minimized = FALSE, position = "bottomleft") 
  }
  
  return(map)
  
}




















