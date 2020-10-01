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

# radius of POI points
pointrad = 5

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



     map = get_basemap(maptitle=maptitle) %>%
           add_map_polygons(db, db_metric, pal) #%>%
           #add_map_points_of_interest(db=points_of_interest,pal=pal_points_of_interest,pointrad=pointrad)

  mapshot(map, file=filename)
}

##################################
# for weekly updates

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

##################################
# for publication

make_picture_sequence = function() {

  mydates = seq(mdy("2/2/2020"), today(), by="month")

  for(i in 1:length(mydates)) {
    dt = mydates[i]
    fname = paste(format(dt, "%d%b%Y"), "_town_contact.png", sep="")
    print(fname)
    maptitle = paste("Contact at the town level on", format(dt, "%B %d, %Y"))
    cat("############################\n")
    cat("Writing", fname, "\n")
    map_picture(dt, "town", "contact", maptitle=maptitle, filename=fname)
  }
  
  make_state_contact_plot(plot_date=mydates, show_last_month=FALSE)
  dev.print(device=pdf, file="state_contact_plot.pdf")
}

######################################
# Make the state contact trace plot 

make_state_contact_plot = function(plot_date=NULL, show_last_month=TRUE) {
  contact_max = max(dat_by_state$prob_sum)

  plot(dat_by_state$date, dat_by_state$prob_sum, type="l", col="green", lwd=3, xlim=c(min(dat_by_state$date)-1, max(dat_by_state$date)+1),
       ylim=c(0,max(dat_by_state$prob_sum)*1.1), xlab="", ylab="Contacts", axes=FALSE, main="Connecticut total")
  dateseq = c(seq(min(dat_by_state$date), max(dat_by_state$date), by="month"), max(dat_by_state$date))
  axis(1,at=dateseq, lab=format(dateseq, "%b %d"))
  axis(2)
  for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
  if(!is.null(plot_date) || length(plot_date)>0) {
    abline(v=plot_date)
    text(plot_date, contact_max, format(plot_date, "%b %d"), pos=2) 
    for(pd in plot_date) {
      points(pd, dat_by_state$prob_sum[dat_by_state$date == pd], pch=16)
      text(pd, dat_by_state$prob_sum[dat_by_state$date == pd], format(dat_by_state$prob_sum[dat_by_state$date == pd], digits=1), pos=4)
    }
  }

  if(show_last_month) {
    # last month
    par(mar=c(2.5,2,1,1)) 
    dat_by_state_lastmonth = filter(dat_by_state, date > max(dat_by_state$date)-30)
    plot(dat_by_state_lastmonth$date, dat_by_state_lastmonth$prob_sum, type="l", col="green", lwd=3, 
         xlim=c(max(dat_by_state$date)-30, max(dat_by_state$date)+3),
         #ylim=c(0,max(dat_by_state_lastmonth$prob_sum)*1.1), 
         xlab="", 
         ylab="", #"Contacts", 
         axes=FALSE, main="Connecticut total (last 30 days)")
    dateseq_lastmonth = c(seq(min(dat_by_state_lastmonth$date), max(dat_by_state_lastmonth$date), by="week"), max(dat_by_state$date))
    axis(1,at=dateseq_lastmonth, lab=format(dateseq_lastmonth, "%b %d"))
    axis(2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    if(!is.null(plot_date)) {
      abline(v=plot_date)
      text(plot_date, contact_max, format(plot_date, "%b %d"), pos=2) 
      points(plot_date, dat_by_state$prob_sum[dat_by_state$date == plot_date], pch=16)
      text(plot_date, dat_by_state$prob_sum[dat_by_state$date == plot_date], format(dat_by_state$prob_sum[dat_by_state$date == plot_date], digits=1), pos=4)
    }
  }
}

###########################################
# make area contact trace plot

make_area_contact_plot = function(plot_date=NULL, metric_type, area_level, area_id=NULL) {

  if(metric_type == "home") {
      if(area_level == "town") {
        db = dat_merged_towns
        db_metric = dat_merged_towns$prob_sum_home
      } else {
        db = dat_merged_cbgs
        db_metric = dat_merged_cbgs$prob_sum_home
      }
    } else if(metric_type == "contact") {
      if(area_level == "town") {
        db = dat_merged_towns
        db_metric = dat_merged_towns$prob_sum_contact
      } else {
        db = dat_merged_cbgs
        db_metric = dat_merged_cbgs$prob_sum_contact
      }
    } else {
      stop("invalid metric type")
    }

    if(is.null(area_id) || !(area_id %in% db$area_name)) {
      if(area_level == "town") {
        area_id = "New Haven"
      } else {
        area_id = "New Haven-090091403002"
      }
    }


    dat_by_state_lastmonth = filter(dat_by_state, date > max(dat_by_state$date)-30)

    contact_max = max(db_metric[db$area_name ==  area_id])
    par(mar=c(2.5,4,1,1)) 
    plot(0, type="n", xlim=c(min(db$date)-1, max(db$date)+1),
             ylim=c(0,max(db_metric[db$area_name ==  area_id]*1.1)), xlab="", 
             ylab="Contacts", axes=FALSE, main=area_id)
    dateseq = c(seq(min(dat_by_state$date), max(dat_by_state$date), by="month"), max(dat_by_state$date))
    axis(1,at=dateseq, lab=format(dateseq, "%b %d"))
    axis(2)
    lines(db$date[db$area_name ==  area_id], db_metric[db$area_name == area_id], col="black", lwd=2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    if(!is.null(plot_date)) {
      abline(v=plot_date)
      text(plot_date, contact_max, format(plot_date, "%b %d"), pos=2) 
      points(plot_date, db_metric[db$area_name == area_id & db$date == plot_date], pch=16)
      text(plot_date, db_metric[db$area_name == area_id & db$date == plot_date], 
          format(db_metric[db$area_name == area_id & db$date == plot_date], digits=1), pos=4)
    }

    # last month
    par(mar=c(2.5,2,1,1)) 
    db_lastmonth = filter(db, date > max(db$date)-30)
    db_metric_lastmonth = db_metric[db$date > max(db$date)-30]
    dateseq_lastmonth = c(seq(min(dat_by_state_lastmonth$date), max(dat_by_state_lastmonth$date), by="week"), max(dat_by_state$date))
    plot(0, type="n", 
             xlim=c(min(dateseq_lastmonth), max(dateseq_lastmonth)+3),
             ylim=range(db_metric_lastmonth[db_lastmonth$area_name == area_id]),
             xlab="", 
             ylab="", #"Contacts", 
             axes=FALSE, main=paste(area_id, "(last 30 days)"))
    axis(1,at=dateseq_lastmonth, lab=format(dateseq_lastmonth, "%b %d"))
    axis(2)
    lines(db_lastmonth$date[db_lastmonth$area_name ==  area_id], db_metric_lastmonth[db_lastmonth$area_name == area_id], col="black", lwd=2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    if(!is.null(plot_date)) {
      abline(v=plot_date)
      text(plot_date, max(db_metric_lastmonth), format(plot_date, "%b %d"), pos=2) 
      if(plot_date > min(dateseq_lastmonth)) {
        points(plot_date, db_metric_lastmonth[db_lastmonth$area_name == area_id & db_lastmonth$date == plot_date], pch=16)
        text(plot_date, db_metric_lastmonth[db_lastmonth$area_name == area_id & db_lastmonth$date == plot_date], 
              format(db_metric_lastmonth[db_lastmonth$area_name == area_id & db_lastmonth$date == plot_date], digits=1), pos=4)
      }
    }

}









