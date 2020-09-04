
### About this application

This interactive web application shows the locations of close social contacts, and the residence locations of individuals having close contacts, in Connecticut.  A close social contact occurs when two devices are within two meters of one another.  Because device location data comes with substantial uncertainty, we use a statistical algorithm for computing the probability that the devices are within two meters. Close contacts occurring at residence locations and major roadways are excluded.  The data shown here are derived from anonymized mobile device locations aggregated at the town and [census block group](https://en.wikipedia.org/wiki/Census_block_group) levels.  No individually identifiable location information is presented. 

The web app uses [leaflet](https://leafletjs.com) and [Shiny](https://shiny.rstudio.com/). The map explorer is based in part on [Edward Parker](https://www.lshtm.ac.uk/aboutus/people/parker.edward)'s [COVID-19 tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) and [code](https://github.com/eparker12/nCoV_tracker). 


