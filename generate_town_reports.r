library(rmarkdown)
library(stringr)

load("ct_data.Rsave")

######################
# read town names from file 
town_names = sort(ct_towns$NAME)
town_names = town_names[town_names != "County Subdivisions Not Defined"]

# creates only if the dir does not already exist
dir.create(file.path(".", "reports"), showWarnings = FALSE)

########################

# testing: 
town_names = c("Danbury", "New Haven", "Hartford", "Bridgeport", "Stamford", "Waterbury")


ctr = 1

start_time <- Sys.time()

for(town_name in town_names) {

  cat("##################################\nWorking on", town_name, " (", ctr, "of", length(town_names), ") \n")

  town_name_file = str_replace(town_name, " ", "_")

  dir.create(file.path("reports", town_name_file), showWarnings = FALSE)

  render("town_report.Rmd", 
         param=list(town=town_name),
         output_file=paste("reports/", town_name_file, "/index.html", sep=""))

  ctr = ctr + 1

}


end_time <- Sys.time()

etime = end_time - start_time

cat("Time to complete processing of towns:\n", format(etime), "\n")


#################
# generate state report

dir.create(file.path("reports", "Connecticut"), showWarnings = FALSE)

render("state_report.Rmd", output_file="reports/Connecticut/index.html")


################
# write index file with list of towns 

town_strings = paste("[",town_names,"](", str_replace(town_names, " ", "_"), ")\n", sep="")

fileConn <- file("reports/index.md")
writeLines("[Connecticut](Connecticut)\n\n", fileConn)
writeLines(town_strings, fileConn)
close(fileConn)


