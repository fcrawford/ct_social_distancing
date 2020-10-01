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

  output$contact_curve <- renderPlot({
    par(mar=c(2.5,4,1,1)) 
    layout(matrix(c(1,2,3,4),nrow=2, byrow=TRUE), widths=c(2,1), heights=1)
    make_state_contact_plot(input$plot_date)
    make_area_contact_plot(input$plot_date, input$metric_type, input$area_level, area_id=input$mymap_shape_click$id)
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

    leafletProxy("mymap") %>% 
    clearMarkers() %>%
    clearShapes() %>%
    clearControls() %>% 
    add_map_polygons(db, db_metric, pal) %>%
    add_map_points_of_interest(db=points_of_interest,pal=pal_points_of_interest,pointrad=pointrad) %>% 
    addLegend("bottomright", pal = pal, values = db_metric, title = metric_lab) 

  })
}

#########################

shinyApp(ui, server)




