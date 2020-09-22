library(mapview)
library(leaflet) 
library(DT)
library(sf)
library(dplyr)
library(lubridate)

if(!("dat_merged_towns" %in% ls())) {
  load("ct_data.Rsave")
}

###########################
# Color palettes

pal_town_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,30,by=5),Inf)) 
pal_cbg_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,8,by=1),Inf)) 
pal_town_home = colorBin("Blues", domain=NULL, bins=c(seq(0,30,by=5),Inf)) 
pal_cbg_home = colorBin("Blues", domain=NULL, bins=c(seq(0,8,by=1),Inf)) 

#poi_types = c("Hospital", "House of Worship", "Sports Venue", "Mall", "University", "Casino", 
              #"Supermarket", "Grade School", "Kindergarten / Daycare / Pre-k",
              #"Universities", "College / Tradeschool", "Park / Historic Site", "Court")

poi_types = unique(points_of_interest$type)

pal_points_of_interest = colorFactor(palette = rainbow(length(poi_types)), #c("blue", "orange", "purple", "red", "yellow"), 
                                     levels = poi_types)

###################################
# days of week

saturdays = seq(mdy("2/1/2020"), today()+7, by="week")
sundays = seq(mdy("2/2/2020"), today()+7, by="week")


#############################

get_basemap = function(maptitle="") {

    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title { 
      #position: fixed !important;
      #right: 50%;
      #text-align: center;
      #left: 50px;
      #top: 25px;
      align: center;
      padding-left: 10px; 
      padding-right: 10px; 
      #background: rgba(255,255,255,0.75);
      font-weight: bold;
      font-size: 28px;
    }
  "))

  title <- tags$div( tag.map.title, HTML(maptitle))


  basemap = leaflet() %>% 
          addLayersControl(
            position = "topright",
            baseGroups = c("CartoDB.Positron", "Esri.WorldStreetMap", "CartoDB.Voyager", "Esri.WorldImagery"),
            overlayGroups = c("Metric", "Points of Interest"),
            options = layersControlOptions(collapsed = TRUE)) %>% 
          hideGroup(c("Points of Interest")) %>%
          addProviderTiles("CartoDB.Positron", group="CartoDB.Positron") %>% 
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addProviderTiles("Esri.WorldStreetMap", group="Esri.WorldStreetMap") %>%
          addProviderTiles("CartoDB.Voyager", group="CartoDB.Voyager") %>%
          setView(-72.694684, 41.538881, zoom=9) %>% # Middletown
          addControl(title, position = "topleft", className="map-title")


  return(basemap)
}

#############################


add_map_polygons = function(map, db, db_metric, pal) {


  mapadd = addPolygons(map, data = db, stroke = TRUE, weight=1, color = "#999", smoothFactor = 0.1, fillOpacity = 0.8, 
                fillColor = ~pal(db_metric),
                group = "Metric",
                layerId = ~area_name,
                label = sprintf("<strong>%s</strong><br/>%g", db$area_name, db_metric) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = "green"),
                    textsize = "15px", direction = "auto"),
                highlight = highlightOptions(stroke=TRUE, weight=6, bringToFront=FALSE, color = "#999"))   

  return(mapadd)
}

###############################


add_map_points_of_interest = function(map, db,pal, pointrad) {

  mapadd = addCircleMarkers(map, data=db, lat = ~ lat, lng = ~ lon, 
                 weight = 1, stroke=FALSE, radius = pointrad, fillOpacity = 0.3, 
                 group = "Points of Interest",
                 color = ~pal(type),
                 label = sprintf("<strong>%s: %s</strong><br/>%s %s", 
                                 db$type, db$name, db$address, db$city) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color"=~pal(db$type)), # color does not work
                   textsize = "15px", direction = "auto"))
  return(mapadd)
}


###############################

map_picture = function(plot_date, area_level, metric_type, maptitle, filename) {


    if(metric_type == "home") {
      if(area_level == "town") {
        db = filter(dat_merged_towns, date == plot_date)
        db_metric = db$prob_sum_home
        pal = pal_town_home
      } else {
        db = filter(dat_merged_cbgs, date == plot_date)
        db_metric = db$prob_sum_home
        pal = pal_cbg_home
      }
    } else if(metric_type == "contact") {
      if(area_level == "town") {
        db = filter(dat_merged_towns, date == plot_date)
        db_metric = db$prob_sum_contact
        pal = pal_town_contact
      } else {
        db = filter(dat_merged_cbgs, date == plot_date)
        db_metric = db$prob_sum_contact
        pal = pal_cbg_contact
      }
    } else {
      stop("invalid metric type")
    }

    pointrad = 5


     map = get_basemap(maptitle=maptitle) %>%
           add_map_polygons(db, db_metric, pal) %>%
           add_map_points_of_interest(db=points_of_interest,pal=pal_points_of_interest,pointrad=pointrad)

  mapshot(map, file=filename)
}

##################################

make_map_pictures = function(plot_date=NULL) {

  if(is.null(plot_date)) {
    plot_date = today() - 7
  }

  for(area_level in c("town", "cbg")) {
    for(metric_type in c("contact", "home")) {
      fname = paste(format(plot_date, "%d%b%Y"), "_", area_level, "_", metric_type, ".png", sep="")
      maptitle = paste(ifelse(metric_type == "contact","Contact", "Home location of contact"), 
                       "at the", 
                       ifelse(area_level == "town","town", "census block group"), 
                       "level on", 
                       format(plot_date, "%B %d, %Y"))
      cat("############################\n")
      cat("Writing", fname, "\n")
      map_picture(plot_date, area_level, metric_type, maptitle=maptitle, filename=fname)
    }
  }
}





