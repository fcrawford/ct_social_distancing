library(rmarkdown)

load("ct_data.Rsave")

######################
# read town names from file 
town_names = ct_towns$NAME
town_names = town_names[town_names != "County Subdivisions Not Defined"]

dir.create(file.path(".", "reports"), showWarnings = FALSE)

########################

# testing: 
town_names = c("Danbury", "New Haven", "Hartford", "Bridgeport", "Stamford", "Waterbury")

ctr = 1

start_time <- Sys.time()

for(town_name in town_names) {

  cat("##################################\nWorking on", town_name, " (", ctr, "of", length(town_names), ") \n")

  town_name_file = str_replace(town_name, " ", "_")

  render("town_report.Rmd", 
         param=list(town=town_name),
         output_file=paste("reports/", town_name_file, "_report.html", sep=""))

  ctr = ctr + 1
}


end_time <- Sys.time()

etime = end_time - start_time

cat("Time to complete processing of towns:\n", format(etime), "\n")


