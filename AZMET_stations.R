###Travis Zalesky
#4/6/2024
#V2.0.0

#Version History:
## V1.0.0 - Initial Commit
## V2.0.0 - Major revision for UA GIST 602B - Lab 4

#Objectives: 
### Bulk download data from https://ag.arizona.edu/AZMET/
### Query weather stations surrounding Phoenix
### Parse XML data, target wind and sol rad data

#Setup ----
#Packages
###install.packages("RCurl") #Run only if necessary, note out after installation.
library(RCurl) #Attach package
###install.packages("XML") #Run only if necessary, note out after installation.
library(XML) #Attach package
###install.packages("tidyverse") #Run only if necessary, note out after installation.
library(tidyverse) #Attach package
###install.packages("terra") #Run only if necessary, note out after installation.
#library(terra) #Attach package

#Set Working Directory (wd)
dir.string <- "C:/GIS_Projects/602B/Lab4/Windspeed/R"
setwd(dir.string)

#Create Data folder in wd
dir.create("Data")

azmetURL <- "https://ag.arizona.edu/AZMET/"

# Get list of sub-pages in AZMET homepage
azmetDir <- getURL(azmetURL) %>%
  htmlParse(asText=TRUE) %>%
  xpathSApply('//td/a', xmlGetAttr, 'href')

#Complete full url for internal (.htm) links
for (href in azmetDir) {
  #print(href)
  if (grepl(".htm$", href)) {
    azmetDir[grep(href, azmetDir)] <- paste(azmetURL, href, sep = "")
  }
}

#Historical data = azmetDir[8] = "https://ag.arizona.edu/AZMET/az-data.htm"
stationPage <- azmetDir[grep(".az-data.htm$", azmetDir)]

# Get list of sub-pages in AZMET stations page
stationsDir <- getURL(stationPage) %>%
  htmlParse(asText=TRUE) %>%
  xpathSApply('//a', xmlGetAttr, 'href')
##NOTE: station directory .htm names are not human readable.

#Identify weather stations of interest
targetStations <- as.character(c(1:43)) #All weather stations
for (i in targetStations) {
  if (str_length(i) == 1) {
    targetStations[as.numeric(i)] <- paste("0", i, sep = "")
  }
}
  
  
#Create dataframe for station data
stationsDf <- data.frame(matrix(NA, nrow = 1, ncol = 5))
names(stationsDf) <- c("URL", "Lat", "Lon", "Number", "Name")

# Get AZMET station specific XML
test_XML <- getURL(paste(azmetURL, stationsDir[grepl(targetStations[1], stationsDir)], 
                         sep = "")) %>%
  htmlParse(asText=TRUE) 

test_XML

test_head <- test_XML%>%
  xpathSApply('//font/b', xmlValue) 

name <- test_head[2]

number <- targetStations[1]

url <- paste(azmetURL, stationsDir[grepl(targetStations[1], stationsDir)], 
             sep = "")

#Station metadata
test_MD <- test_XML%>%
  xpathSApply('//ul', xmlValue)%>%
  str_split("\n")
test_MD <- test_MD[[1]]

test_Coords <- str_split(test_MD[8], ", ")[[1]]

Lat <- test_Coords[1]
Lon <- test_Coords[2]

row <- c(name, number, url, Lat, Lon)
names(row) <- c("Name", "Number", "URL", "Lat", "Lon")

stationsDf <- stationsDf%>%
  bind_rows(row)%>%
  filter(!is.na(Name))

#Set up looping function for all remaining target stations
#Station coordinates are too unpredictable to be reliably automated.
##Cycle through loop manually, and modify coords object according to MD
station <- targetStations[43]
  # Get AZMET station specific XML
  XML <- getURL(paste(azmetURL, stationsDir[grepl(station, stationsDir)], 
                           sep = "")) %>%
    htmlParse(asText=TRUE) 
  
  XML
  
  head <- XML%>%
    xpathSApply('//font/b', xmlValue) 
  
  name <- head[2]
  
  number <- station
  
  url <- paste(azmetURL, stationsDir[grepl(station, stationsDir)], 
               sep = "")[1]
  
  #Station metadata
  MD <- XML%>%
    xpathSApply('//ul', xmlValue)%>%
    str_split("\n")
  MD <- MD[[1]]
  
  MD
  
  Coords <- str_split(MD[6], ", ")[[1]]
  
  Lat <- Coords[1]
  Lon <- Coords[2]
  
  row <- c(name, number, url, Lat, Lon)
  names(row) <- c("Name", "Number", "URL", "Lat", "Lon")
  
  stationsDf <- stationsDf%>%
    bind_rows(row)
  

  
stationsDf <- stationsDf%>%
  mutate(Lat = parse_number(Lat),
         Lon = parse_number(Lon))

write.csv(stationsDf, "Data/Stations.csv", row.names = F)

stationsDf <- read.csv("Data/Stations.csv")%>%
         mutate(meta_station_id = paste("az", Number),
                meta_station_name = Name)

stationsDf <- stationsDf%>%
  mutate(meta_station_id = if_else(str_length(meta_station_id) == 4,
                                     str_replace(meta_station_id, " ", "0"),
                                     str_replace(meta_station_id, " ", "")))

stationsDf <- stationsDf%>%
  select(-Name, -Number)

library(azmetr)

date <- as.Date("2023-05-14")

data <- az_daily(start_date = date, end_date = date)

data <- data%>%
  left_join(stationsDf, by = c("meta_station_id", "meta_station_name"))

write.csv(data, "Data/data_full.csv", row.names = F)

data_slim <- data%>%
  select(meta_station_id, meta_station_name, Lat, Lon, date_doy, date_year,
         datetime, eto_pen_mon, precip_total_mm, sol_rad_total,
         temp_air_maxC, temp_air_minC, temp_air_meanC, wind_2min_spd_max_mps,
         wind_2min_spd_mean_mps, wind_2min_timestamp, wind_2min_vector_dir,
         wind_spd_max_mps, wind_spd_mean_mps, wind_vector_dir,
         wind_vector_dir_stand_dev, wind_vector_magnitude)

write.csv(data_slim, "Data/data_slim.csv", row.names = F)


#Get station elevation data
elevationDF <- data.frame(matrix(NA, nrow = 1, ncol = 4))
names(elevationDF) <- c("Name", "Number", "Elevation", "Warn")

for (station in c(26:43)) {
  print(station)

# Get AZMET station specific XML
XML <- getURL(paste(azmetURL, stationsDir[grepl(station, stationsDir)], 
                    sep = "")) %>%
  htmlParse(asText=TRUE) 

XML

head <- XML%>%
  xpathSApply('//font/b', xmlValue) 

name <- head[2]

number <- station

#Station metadata
MD <- XML%>%
  xpathSApply('//ul', xmlValue)%>%
  str_split("\n")
MD <- MD[[1]]

MD

elevation <- parse_number(MD[grep("Elevation :", MD)])
if (length(elevation) != 1){
  warn <- T
  if (length(elevation) > 1) {
    elevation <- elevation[1]
  } else {
    elevation <- NA
  }
} else {
  warn <- F
}

row <- c(name, number, elevation, warn)
names(row) <- c("Name", "Number", "Elevation", "Warn")

elevationDF <- elevationDF%>%
  bind_rows(row)
}

#Warn = T locations manually checked for accuracy. Updated as warrented
elevationDF <- elevationDF%>%
  filter(!is.na(Name))%>%
  mutate(Number = if_else(str_length(Number) == 2,
                          paste('az', Number, sep = ""),
                          paste('az', Number, sep = "0")))
write.csv(elevationDF, "Data/stations_elev.csv", row.names = F)

stationsDf <- stationsDf%>%
  left_join(elevationDF, by = "Number")

stationsDf <- stationsDf%>%
  select(Name.x, Number, URL, Lat, Lon, Elevation)%>%
  rename(Name = Name.x)

stationsDf <- stationsDf%>%
  rename(meta_station_id = Number,
         meta_station_name = Name)

stationsDf <- stationsDf%>%
  select(-meta_station_name)

data <- data%>%
  left_join(stationsDf, by = "meta_station_id")

summary(data$Elevation)
summary(data$Lat)
summary(data$Lon)

temp <- data%>%
  select(meta_station_id, meta_station_name, Elevation)
