#Created by Justin Mathias 4/30/2024 for Kinsey Reed
library(easypackages)
libraries(c("tidyverse","openxlsx"))

dat <- read.xlsx("/Users/justinmathias/Library/CloudStorage/Dropbox/WVU Faculty/Research/Collaborating On/KinseyPlants/Data/all_data.xlsx")

#Define useful functions
#Parsing out lat/lon----
sep.coords <- function(dat, in_col, return = "Coordinates") {
  if (return == "Coordinates"){
    defaultW <- getOption("warn")
    options(warn = -1)
    dat %>% separate({{in_col}}, into = c("Lat", "Lon"), sep = "\\/|\\,", convert = TRUE) #Separate can only handle one argument. Use regex to do the job
  } else if (return == "Lat") {
    defaultW <- getOption("warn")
    options(warn = -1)
    sep <- dat %>% separate({{in_col}}, into = c("Lat", "Lon"), sep = "\\/|\\,", convert = TRUE)
    options(warn = defaultW)
    return(sep$Lat)
  } else if (return == "Lon") {
    defaultW <- getOption("warn")
    options(warn = -1)
    sep <- dat %>% separate({{in_col}}, into = c("Lat", "Lon"), sep = "\\/|\\,", convert = TRUE)
    options(warn = defaultW)
    return(sep$Lon)
  } else {
    return("Must be Coordinates, Lat, or Lon")
  }
}

#Separate coordinates into lat/lon
Dat <- dat %>% 
  sep.coords(Coordinates)
