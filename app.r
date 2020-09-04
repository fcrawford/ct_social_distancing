library(leaflet) 
library(tigris)
library(lubridate)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinythemes)
library(DT)

#####################################
# load data

load("ct_data.Rsave")


###################################
# days of week

saturdays = seq(mdy("2/1/2020"), today(), by="week")
sundays = seq(mdy("2/2/2020"), today(), by="week")

#######################

daymin = min(dat_merged_towns$date)
daymax = max(dat_merged_towns$date)


###################################
# create the base map

# todo: put this in util.r / get_basemap()

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
          #setView(-72.694684, 41.538881, zoom=9) %>% # Middletown
          setView(-73.011905, 41.289133, zoom=9) %>% # New Haven/Milford
          addEasyButton(easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }")))


pal_town_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,30,by=5),Inf)) 
pal_cbg_contact = colorBin("Greens", domain=NULL, bins=c(seq(0,8,by=1),Inf)) 
pal_town_home = colorBin("Blues", domain=NULL, bins=c(seq(0,30,by=5),Inf)) 
pal_cbg_home = colorBin("Blues", domain=NULL, bins=c(seq(0,8,by=1),Inf)) 

pal_points_of_interest = colorFactor(palette = c("blue", "orange", "purple", "red"), 
                                     levels = c("Hospital", "House of Worship", "Sports Venue", "Mall"))


#############################

# todo: put all this in clean_data.r 


# town data from prior 2 weeks


dat_by_town_lastweek_mean = aggregate(prob_sum_contact ~ town, data=dat_by_town_lastweek, mean)
dat_by_town_lastweek2_mean = aggregate(prob_sum_contact ~ town, data=dat_by_town_lastweek2, mean)

ntowns = dim(dat_by_town_lastweek_mean)[1]

dat_by_town_lastweek_mean$rank = ntowns + 1 - rank(dat_by_town_lastweek_mean$prob_sum_contact)
dat_by_town_lastweek2_mean$rank = ntowns + 1 - rank(dat_by_town_lastweek2_mean$prob_sum_contact)

dat_by_town_week_comparison = merge(dat_by_town_lastweek_mean, dat_by_town_lastweek2_mean, by="town", suffixes=c("_lastweek", "_lastweek2"))

rownames(dat_by_town_week_comparison) <- NULL

ord = order(dat_by_town_week_comparison$prob_sum_contact_lastweek, decreasing=TRUE)

dat_by_town_week_comparison = dat_by_town_week_comparison[ord,]

names(dat_by_town_week_comparison) <- c("Town", "Contact this week", "Rank this week", "Contact last week", "Rank last week")

###############################
# cbg data from prior 2 weeks 


dat_by_cbg_lastweek_mean = aggregate(prob_sum_contact ~ geoid, data=dat_by_cbg_lastweek, mean)
dat_by_cbg_lastweek2_mean = aggregate(prob_sum_contact ~ geoid, data=dat_by_cbg_lastweek2, mean)

ncbgs = dim(dat_by_cbg_lastweek_mean)[1]

dat_by_cbg_lastweek_mean$rank = ncbgs + 1 - rank(dat_by_cbg_lastweek_mean$prob_sum_contact)
dat_by_cbg_lastweek2_mean$rank = ncbgs + 1 - rank(dat_by_cbg_lastweek2_mean$prob_sum_contact)

dat_by_cbg_week_comparison = merge(dat_by_cbg_lastweek_mean, dat_by_cbg_lastweek2_mean, by="geoid", suffixes=c("_lastweek", "_lastweek2"))

rownames(dat_by_cbg_week_comparison) <- NULL

ord = order(dat_by_cbg_week_comparison$prob_sum_contact_lastweek, decreasing=TRUE)

dat_by_cbg_week_comparison = dat_by_cbg_week_comparison[ord,]

# need to merge with ct_cbgs, which contains poi strings 

ct_cbgs_pois = ct_cbgs[,c("GEOID", "poi", "area_name")]
st_geometry(ct_cbgs_pois) <- NULL
names(ct_cbgs_pois) = c("geoid", "poi", "area_name")

dat_by_cbg_week_comparison = merge(dat_by_cbg_week_comparison, ct_cbgs_pois, by="geoid")

#print(head(dat_by_cbg_week_comparison))

#stop("here")

names(dat_by_cbg_week_comparison) <- c("CBG", "Contact this week", "Rank this week", "Contact last week", "Rank last week", "Points of interest", "Town-CBG")

dat_by_cbg_week_comparison = dat_by_cbg_week_comparison[,c("Town-CBG", "Points of interest", "Contact this week", "Rank this week", "Contact last week", "Rank last week")]

ord = order(dat_by_cbg_week_comparison$`Contact this week`, decreasing=TRUE)
dat_by_cbg_week_comparison = dat_by_cbg_week_comparison[ord,]



###########################33


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Connecticut Social Contact Trends (beta)", id="nav",
   tabPanel("Explorer", div(class="outer", tags$head(includeCSS("styles.css")),
    leafletOutput("mymap", width="100%", height="100%"),
    #absolutePanel(
      #top = 3, left = 60, 
      #style = "z-index:500; text-align: left;",
      #tags$h2("Connecticut Social Contact Explorer")
    #),
    absolutePanel(id = "controls", class = "panel panel-default",
               top = 75, left = 60, width = 250, fixed=TRUE,
               draggable = TRUE, height = "auto",
               #h4("Controls"),
               radioButtons("area_level", "Spatial resolution", c("Town" = "town", "Census block group" = "cbg")),
               radioButtons("metric_type", "Metric", c("Contact locations" = "contact", "Home locations" = "home")),
               sliderInput("plot_date",
                 label = h5("Select Date"),
                 min = daymin,
                 max = daymax,
                 value = daymax,
                 timeFormat = "%d %b", 
                 animate=animationOptions(interval = 1000, loop = FALSE))),
      absolutePanel(id="lot", class = "panel panel-default",
               bottom = "10%", left = "10%", height = 150, width = 800, #fixed=TRUE, 
               draggable = TRUE, 
                 plotOutput("contact_curve", height="200px", width="800px"))
      )),
  tabPanel("Towns",
  fluidPage(
  fluidRow(
    column(12, 
    includeMarkdown("town_text.md"),
    DT::dataTableOutput('town_table')
    )))), 
  tabPanel("Block groups",
  fluidPage(
  fluidRow(
    column(12, 
    includeMarkdown("cbg_text.md"),
    DT::dataTableOutput('cbg_table')
    )))), 
  tabPanel("About",
  fluidPage(
  fluidRow(
    column(12, 
    includeMarkdown("about_web.md"),
    includeMarkdown("ack.md"),
    includeMarkdown("contact.md")
    )))) 

  ) 
)

###########################

server = function(input, output, session) {




  output$town_table <- DT::renderDataTable(dat_by_town_week_comparison, rownames=FALSE) 

  output$cbg_table <- DT::renderDataTable(dat_by_cbg_week_comparison, rownames=FALSE) 

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


    # this function below is not used right now. 
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




  # abstract this out and put in util.r 
  output$contact_curve <- renderPlot({
    contact_max = max(dat_by_state$prob_sum)
    par(mar=c(3,4,1,1))
    plot(dat_by_state$date, dat_by_state$prob_sum, type="l", col="green", lwd=3, xlim=c(min(dat_by_state$date)-1, max(dat_by_state$date)+1),
         ylim=c(0,max(dat_by_state$prob_sum)), xlab="", ylab="Number of contacts (statewide)", axes=FALSE, main=)
    dateseq = c(seq(min(dat_by_state$date), max(dat_by_state$date), by="month"), max(dat_by_state$date))
    #axis(1,at=dat_town$date, lab=format(dat_town$date, "%b %d"))
    axis(1,at=dateseq, lab=format(dateseq, "%b %d"))
    #axis(1,at=dat_by_state$date, lab=format(dat_by_state$date, "%b %d"))
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

       addCircleMarkers(data=points_of_interest, lat = ~ lat, lng = ~ lon, 
                 weight = 1, stroke=FALSE, radius = pointrad, fillOpacity = 0.3, 
                 group = "Points of Interest",
                 color = ~pal_points_of_interest(type),
                 label = sprintf("<strong>%s: %s</strong><br/>%s %s", points_of_interest$type, points_of_interest$name, 
                             points_of_interest$address, points_of_interest$city) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px", direction = "auto")) %>% 


    #addCircleMarkers(data=hospitals, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~(POPULATION)^(1/4), 
                 #fillOpacity = 0.2, color = "blue", group = "Hospitals",
                 #label = sprintf("<strong>%s</strong><br/>%s %s", hospitals$NAME, hospitals$ADDRESS, hospitals$CITY) %>% lapply(htmltools::HTML),
                 #labelOptions = labelOptions(
                   #style = list("font-weight" = "normal", padding = "3px 8px", "color" = "blue"),
                   #textsize = "15px", direction = "auto")) %>% 
    #addCircleMarkers(data=churches, lat = ~ Y, lng = ~ X, weight = 1, radius = pointrad,
                 #fillOpacity = 0.2, color = "red", group = "Houses of Worship",
                 #label = sprintf("<strong>%s</strong><br/>%s %s", churches$NAME, churches$STREET, churches$CITY) %>% lapply(htmltools::HTML),
                 #labelOptions = labelOptions(
                   #style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   #textsize = "15px", direction = "auto")) %>% 
    #addCircleMarkers(data=malls, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = pointrad,
                 #fillOpacity = 0.2, color = "orange", group = "Malls",
                 #label = sprintf("<strong>%s</strong><br/>%s", malls$NAME, malls$CITY) %>% lapply(htmltools::HTML),
                 #labelOptions = labelOptions(
                   #style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   #textsize = "15px", direction = "auto")) %>% 
   #addCircleMarkers(data=sportsvenues, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = pointrad,
                 #fillOpacity = 0.2, color = "purple", group = "Sports Venues",
                 #label = sprintf("<strong>%s</strong><br/>%s", sportsvenues$NAME, sportsvenues$CITY) %>% lapply(htmltools::HTML),
                 #labelOptions = labelOptions(
                   #style = list("font-weight" = "normal", padding = "3px 8px", "color" = "red"),
                   #textsize = "15px", direction = "auto")) %>%
   addLegend("bottomright", pal = pal, values = db_metric, title = metric_lab) 
                   




  })

}

#########################

shinyApp(ui, server)




