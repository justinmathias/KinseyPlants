#Created by Justin Mathias 4/30/2024 for Kinsey Reed
library(easypackages)
libraries(c("tidyverse","openxlsx","elevatr","terra","sf"))

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

#Load in WV digital elevation model
wv.elv <- rast("/Users/justinmathias/Library/CloudStorage/Dropbox/WVU Faculty/Research/Collaborating On/KinseyPlants/Data/wv_ned_02/wv_ned_02/prj.adf")
wv.elv
WV.elv <- project(wv.elv, "+proj=longlat +datum=WGS84 +no_defs")

#Extract elevation for ClimateNA
Dat1 <- Dat %>% 
  mutate(extract(WV.elv, data.frame(Dat$Lon, Dat$Lat))) %>% 
  select(-ID) %>% 
  rename(Elv_m = wv_ned_02)
Dat1
