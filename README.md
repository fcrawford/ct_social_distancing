# ct_social_distancing

## Overview

This repository contains data and code for exploring geographic and temporal trends in close social contacts, and the residence locations of individuals having close contacts, in Connecticut.  There are two major components: 
* Shiny app
* Rmarkdown reports for state and town. 

### running the app

```r
library(shiny)
runApp()
```

## Deploying the app 

The file `manifest.txt` contains the files to deploy to the shinyapps.io server. This is necessary because the reports in other directories are large and will slow down deployment. 

```r
library(rsconnect)
deployApp(appFileManifest="manifest.txt")
```

## Generating reports

```r
library(rmarkdown)

# generate a report for the state of CT
render("state_report.Rmd", output_file="connecticut_report.html")

# generate a report for one town only
render("town_report.Rmd", param=list(town="New Haven"), output_file="new_haven_report.html")

# generate reports for all towns, in folder `reports`
source("generate_town_reports.r")
```

## Data

The file `ct_data.Rsave` contains data frames and spatial features used to make the maps and tables. 


# TODO

## All

* better neighborhood descriptions for CBGs, e.g. <https://rdrr.io/github/CT-Data-Haven/cwi/man/neighborhood_shapes.html>
* better points of interest: schools, colleges, restaurants, big box stores, DMV, state/municipal buildings
* remove "County subdivisions not defined" from towns data frame
* directory structure for reports, rename files "index.html" within these directories, then deploy to github.io


## Web app

* link shape click to plot update
* plotly line plots? 
* sparklines in tables? 

## Town report

* highlight top 5 CBGs
* sparklines in tables
* are houses of worship relevant to include? Not sure. 


## State report

* state trends
* show town-level maps
* highlight top 5 towns in plot
* show table like in web app




