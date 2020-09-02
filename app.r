library(leaflet) 
library(tigris)
library(lubridate)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinythemes)

#####################################
# load data

load("ct_data.Rsave")


###################################
# days of week

saturdays = seq(mdy("2/1/2020"), today(), by="week")
sundays = seq(mdy("2/2/2020"), today(), by="week")

#######################

daymin = mdy("8/10/2020")
daymax = mdy("8/24/2020")


###################################
# create the base map

basemap = leaflet() %>% 
          addLayersControl(
            position = "topright",
            baseGroups = c("CartoDB.Positron", "Esri.WorldStreetMap", "CartoDB.Voyager", "Esri.WorldImagery"),
            overlayGroups = c("Metric", "Hospitals", "Houses of Worship", "Malls", "Sports Venues"),
            options = layersControlOptions(collapsed = TRUE)) %>% 
          hideGroup(c("Hospitals", "Houses of Worship", "Malls", "Sports Venues")) %>%
          addProviderTiles("CartoDB.Positron", group="CartoDB.Positron") %>% 
          addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
          addProviderTiles("Esri.WorldStreetMap", group="Esri.WorldStreetMap") %>%
          addProviderTiles("CartoDB.Voyager", group="CartoDB.Voyager") %>%
          setView(-72.694684, 41.538881, zoom=9) %>%
          addEasyButton(easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }")))


pal_town_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,30,by=5),Inf)) #dat_merged_towns$prob_sum_contact)
pal_cbg_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,8,by=1),Inf)) #dat_merged_cbgs$prob_sum_contact)
pal_town_home = colorBin("Greens", domain=NULL, bins=c(seq(0,30,by=5),Inf)) #dat_merged_towns$prob_sum_home)
pal_cbg_home = colorBin("Greens", domain=NULL, bins=c(seq(0,8,by=1),Inf)) #dat_merged_cbgs$prob_sum_home)


###########################33


ui <- bootstrapPage(
  div(class="outer", tags$head(includeCSS("styles.css")),
  leafletOutput("mymap", width="100%", height="700px"),
  absolutePanel(
    top = 3, left = 60, 
    style = "z-index:500; text-align: left;",
    tags$h2("Connecticut Social Contact Explorer"),
  ),
  absolutePanel(id = "controls", class = "panel panel-default",
               top = 75, left = 60, width = 250, fixed=TRUE,
               draggable = TRUE, height = "auto",
               h4("Controls"),
               radioButtons("area_level", "Spatial resolution", c("Town" = "town", "Census block group" = "cbg")),
               radioButtons("metric_type", "Metric", c("Contact locations" = "contact", "Home locations" = "home")),
               sliderInput("plot_date",
                 label = h5("Select Date"),
                 min = daymin,
                 max = daymax,
                 value = daymin,
                 timeFormat = "%d %b", 
                 animate=animationOptions(interval = 1000, loop = FALSE))),
  plotOutput("contact_curve", height="300px", width="100%") #,
  #plotOutput("contact_curve_areas", height="300px", width="100%"),
  p("Developed by Forrest W. Crawford and Whitespace Solutions Inc.")
  ) # div class=outer
)

###########################

server = function(input, output, session) {

  output$contact_curve_areas <- renderPlot({

    if(input$metric_type == "home") {
      if(input$area_level == "town") {
        db = dat_merged_towns
        db_metric = dat_merged_towns$prob_sum_home
      } else {
        db = dat_merged_cbgs
        db_metric = dat_merged_cbgs$prob_sum_home
      }
    } else if(input$metric_type == "contact") {
      if(input$area_level == "town") {
        db = dat_merged_towns
        db_metric = dat_merged_towns$prob_sum_contact
      } else {
        db = dat_merged_cbgs
        db_metric = dat_merged_cbgs$prob_sum_contact
      }
    } else {
      stop("invalid metric type")
    }


    contact_max = max(db_metric)
    output$contact_curve_area = plot(0, type="n", xlim=c(min(dat_by_state$date)-1, max(dat_by_state$date)+1),
             ylim=c(0,max(db_metric)), xlab="", ylab="Number of contacts detected", axes=FALSE, main=)
    axis(1,at=db$date, lab=format(db$date, "%b %d"))
    axis(2)

    area_names = unique(db$area_name)

    for(area_name in area_names) {
      lines(db$date[db$area_name ==  area_name], db_metric[db$area_name == area_name], col=rgb(0,0,0,alpha=0.3))
    }

    # this does not work! 
    if(!is.null(input$mymap_shape_clicked$id)) {
      lines(db$date[db$area_name ==  input$mymap_shape_clicked$id], db_metric[db$area_name == input$mymap_shape_clicked$id], col="red", lwd=2)
    }

    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.2), border=NA) }

    abline(v=input$plot_date)
    text(input$plot_date, contact_max, format(input$plot_date, "%b %d"), pos=2) 
    points(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], pch=16)
    text(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], format(dat_by_state$prob_sum[dat_by_state$date == input$plot_date], digits=1), pos=4)
  })




  output$contact_curve <- renderPlot({
    contact_max = max(dat_by_state$prob_sum)
    plot(dat_by_state$date, dat_by_state$prob_sum, type="l", col="green", lwd=2, xlim=c(min(dat_by_state$date)-1, max(dat_by_state$date)+1),
         ylim=c(0,max(dat_by_state$prob_sum)), xlab="", ylab="Number of contacts detected statewide", axes=FALSE, main=)
    axis(1,at=dat_by_state$date, lab=format(dat_by_state$date, "%b %d"))
    axis(2)

    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.2), border=NA) }

    abline(v=input$plot_date)
    text(input$plot_date, contact_max, format(input$plot_date, "%b %d"), pos=2) 
    points(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], pch=16)
    text(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], format(dat_by_state$prob_sum[dat_by_state$date == input$plot_date], digits=1), pos=4)
  })

  reactive_db = reactive({
    if(input$area_level == "town") {
      df = dat_merged_towns[dat_merged_towns$date == input$plot_date,]
    } else if(input$area_level == "cbg") {
      df = dat_merged_cbgs[dat_merged_cbgs$date == input$plot_date,]
    } else {
      stop("Invalid area level")
    }

    return(df)

  })


  output$mymap <- renderLeaflet({ 
    basemap
  })

  observe({ 
       print(input$mymap_shape_click$id)
   })
  
  observeEvent(
  {input$plot_date   # this is a hack to get observeEvent to notice if either plot_date or area_level have changed. 
   input$area_level
   input$metric_type}, {

    db = reactive_db()

    if(input$metric_type == "home") {
      db_metric = db$prob_sum_home
      metric_lab = "Home Metric"
      if(input$area_level == "town") {
        pal = pal_town_home
      } else {
        pal = pal_cbg_home
      }
    } else if(input$metric_type == "contact") {
      db_metric = db$prob_sum_contact
      metric_lab = "Contact Metric"
      if(input$area_level == "town") {
        pal = pal_town_contact
      } else {
        pal = pal_cbg_contact
      }

    } else {
      stop("invalid metric type")
    }

    pointrad = 5


    leafletProxy("mymap") %>% 
    clearMarkers() %>%
    clearShapes() %>%
    clearControls() %>% # remove legend
    addPolygons(data = reactive_db(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.8, 
                fillColor = ~pal(db_metric),
                group = "Metric",
                layerId = ~area_name,
                label = sprintf("<strong>%s</strong><br/>%g", reactive_db()$area_name, db_metric) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = "green"),
                    textsize = "15px", direction = "auto")) %>% 
    addCircleMarkers(data=hospitals, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~(POPULATION)^(1/4), 
                 fillOpacity = 0.2, color = "blue", group = "Hospitals",
                 label = sprintf("<strong>%s</strong><br/>%s %s", hospitals$NAME, hospitals$ADDRESS, hospitals$CITY) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "blue"),
                   textsize = "15px", direction = "auto")) %>% 
    addCircleMarkers(data=churches, lat = ~ Y, lng = ~ X, weight = 1, radius = pointrad,
                 fillOpacity = 0.2, color = "red", group = "Houses of Worship",
                 label = sprintf("<strong>%s</strong><br/>%s %s", churches$NAME, churches$STREET, churches$CITY) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   textsize = "15px", direction = "auto")) %>% 
    addCircleMarkers(data=malls, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = pointrad,
                 fillOpacity = 0.2, color = "orange", group = "Malls",
                 label = sprintf("<strong>%s</strong><br/>%s", malls$NAME, malls$CITY) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   textsize = "15px", direction = "auto")) %>% 
   addCircleMarkers(data=sportsvenues, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = pointrad,
                 fillOpacity = 0.2, color = "purple", group = "Sports Venues",
                 label = sprintf("<strong>%s</strong><br/>%s", sportsvenues$NAME, sportsvenues$CITY) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   textsize = "15px", direction = "auto")) %>%
   addLegend("bottomright", pal = pal, values = db_metric, title = metric_lab) 
                   




  })

}

#########################

shinyApp(ui, server)




