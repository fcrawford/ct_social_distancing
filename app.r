library(leaflet) 
library(tigris)
library(lubridate)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinythemes)
library(DT)
library(sf)
library(dplyr)

source("util.r")

#####################################
# load data

load("ct_data.Rsave")


#######################

daymin = min(dat_merged_towns$date)
daymax = max(dat_merged_towns$date)


###################################
# create the base map


basemap = get_basemap()


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
      absolutePanel(id="plot", class = "panel panel-default", # plot panel
               bottom = "2%", left = "2%", height = 275, width = 900, 
               draggable = TRUE, 
                 plotOutput("contact_curve", height="275", width="900px"))
      )),
  tabPanel("Towns",
  fluidPage(
  fluidRow(
    column(12, 
    includeMarkdown("town_text.md"),
    DT::dataTableOutput('town_table') #,
    #downloadButton("downloadTownData", "Download town data")
    )))), 
  tabPanel("Block groups",
  fluidPage(
  fluidRow(
    column(12, 
    includeMarkdown("cbg_text.md"),
    DT::dataTableOutput('cbg_table') #,
    #downloadButton("downloadCBGData", "Download block group data")
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

  # abstract this out and put in util.r 
  output$contact_curve <- renderPlot({
    contact_max = max(dat_by_state$prob_sum)
    par(mar=c(2.5,4,1,1)) 
    layout(matrix(c(1,2,3,4),nrow=2, byrow=TRUE), widths=c(2,1), heights=1)

    # state, full range
    plot(dat_by_state$date, dat_by_state$prob_sum, type="l", col="green", lwd=3, xlim=c(min(dat_by_state$date)-1, max(dat_by_state$date)+1),
         ylim=c(0,max(dat_by_state$prob_sum)*1.1), xlab="", ylab="Contacts", axes=FALSE, main="Connecticut total")
    dateseq = c(seq(min(dat_by_state$date), max(dat_by_state$date), by="month"), max(dat_by_state$date))
    axis(1,at=dateseq, lab=format(dateseq, "%b %d"))
    axis(2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    abline(v=input$plot_date)
    text(input$plot_date, contact_max, format(input$plot_date, "%b %d"), pos=2) 
    points(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], pch=16)
    text(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], format(dat_by_state$prob_sum[dat_by_state$date == input$plot_date], digits=1), pos=4)

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
    abline(v=input$plot_date)
    text(input$plot_date, contact_max, format(input$plot_date, "%b %d"), pos=2) 
    points(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], pch=16)
    text(input$plot_date, dat_by_state$prob_sum[dat_by_state$date == input$plot_date], format(dat_by_state$prob_sum[dat_by_state$date == input$plot_date], digits=1), pos=4)

    # area plot
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

    id_click = input$mymap_shape_click$id
    if(is.null(id_click) || !(id_click %in% db$area_name)) {
      if(input$area_level == "town") {
        id_click = "New Haven"
      } else {
        id_click = "New Haven-090091403002"
      }
    }

    contact_max = max(db_metric[db$area_name ==  id_click])
    par(mar=c(2.5,4,1,1)) 
    plot(0, type="n", xlim=c(min(db$date)-1, max(db$date)+1),
             ylim=c(0,max(db_metric[db$area_name ==  id_click]*1.1)), xlab="", 
             ylab="Contacts", axes=FALSE, main=id_click)
    dateseq = c(seq(min(dat_by_state$date), max(dat_by_state$date), by="month"), max(dat_by_state$date))
    axis(1,at=dateseq, lab=format(dateseq, "%b %d"))
    axis(2)
    lines(db$date[db$area_name ==  id_click], db_metric[db$area_name == id_click], col="black", lwd=2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    abline(v=input$plot_date)
    text(input$plot_date, contact_max, format(input$plot_date, "%b %d"), pos=2) 
    points(input$plot_date, db_metric[db$area_name == id_click & db$date == input$plot_date], pch=16)
    text(input$plot_date, db_metric[db$area_name == id_click & db$date == input$plot_date], 
          format(db_metric[db$area_name == id_click & db$date == input$plot_date], digits=1), pos=4)

    # last month
    par(mar=c(2.5,2,1,1)) 
    db_lastmonth = filter(db, date > max(db$date)-30)
    db_metric_lastmonth = db_metric[db$date > max(db$date)-30]
    dateseq_lastmonth = c(seq(min(dat_by_state_lastmonth$date), max(dat_by_state_lastmonth$date), by="week"), max(dat_by_state$date))
    plot(0, type="n", 
             xlim=c(min(dateseq_lastmonth), max(dateseq_lastmonth)+3),
             ylim=range(db_metric_lastmonth[db_lastmonth$area_name == id_click]),
             xlab="", 
             ylab="", #"Contacts", 
             axes=FALSE, main=paste(id_click, "(last 30 days)"))
    axis(1,at=dateseq_lastmonth, lab=format(dateseq_lastmonth, "%b %d"))
    axis(2)
    lines(db_lastmonth$date[db_lastmonth$area_name ==  id_click], db_metric_lastmonth[db_lastmonth$area_name == id_click], col="black", lwd=2)
    for(i in 1:length(saturdays)) { rect(saturdays[i]-1/2, 0, sundays[i]+1/2, 5e5, col=rgb(0,0,0,alpha=0.1), border=NA) }
    abline(v=input$plot_date)
    text(input$plot_date, max(db_metric_lastmonth), format(input$plot_date, "%b %d"), pos=2) 
    if(input$plot_date > min(dateseq_lastmonth)) {
      points(input$plot_date, db_metric_lastmonth[db_lastmonth$area_name == id_click & db_lastmonth$date == input$plot_date], pch=16)
      text(input$plot_date, db_metric_lastmonth[db_lastmonth$area_name == id_click & db_lastmonth$date == input$plot_date], 
            format(db_metric_lastmonth[db_lastmonth$area_name == id_click & db_lastmonth$date == input$plot_date], digits=1), pos=4)
    }




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
    add_map_polygons(db, db_metric, pal) %>%
    #addPolygons(data = reactive_db(), stroke = TRUE, weight=1, color = "#999", smoothFactor = 0.1, fillOpacity = 0.8, 
                #fillColor = ~pal(db_metric),
                #group = "Metric",
                #layerId = ~area_name,
                #label = sprintf("<strong>%s</strong><br/>%g", reactive_db()$area_name, db_metric) %>% lapply(htmltools::HTML),
                #labelOptions = labelOptions(
                    #style = list("font-weight" = "normal", padding = "3px 8px", "color" = "green"),
                    #textsize = "15px", direction = "auto"),
                #highlight = highlightOptions(stroke=TRUE, weight=6, bringToFront=FALSE, color = "#999")) %>%
     add_map_points_of_interest(db=points_of_interest,pal=pal_points_of_interest,pointrad=pointrad) %>% 
       #addCircleMarkers(data=points_of_interest, lat = ~ lat, lng = ~ lon, 
                 #weight = 1, stroke=FALSE, radius = pointrad, fillOpacity = 0.3, 
                 #group = "Points of Interest",
                 #color = ~pal_points_of_interest(type),
                 #label = sprintf("<strong>%s: %s</strong><br/>%s %s", points_of_interest$type, points_of_interest$name, 
                             #points_of_interest$address, points_of_interest$city) %>% lapply(htmltools::HTML),
                 #labelOptions = labelOptions(
                   #style = list("font-weight" = "normal", padding = "3px 8px"),
                   #textsize = "15px", direction = "auto")) %>% 
       addLegend("bottomright", pal = pal, values = db_metric, title = metric_lab) 
                   




  })


  #output$downloadTownData <- downloadHandler(
    #filename = function() {
      #"Town_contact_data.csv"
    #},
    #content = function(file) {
      #write.csv(output$town_table
  #)
    #})

}

#########################

shinyApp(ui, server)




