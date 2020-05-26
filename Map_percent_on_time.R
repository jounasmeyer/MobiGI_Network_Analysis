#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  Map_stops_on_time.R
#   Author: Gianna Mueller
#   Date:   23.05.2020    gm
#         
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Preliminaries
rm(list=ls(all=TRUE))     # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# load library
library(leaflet)

# import data
data_path <- 'data/Output20/stat_delay_output_1.csv'
locations <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);
  
# create punctuality range to define the type as follows 
locations$punctualityrange = cut(locations$percent_on_time, 
                              breaks = c(0, 80, 85, 90, 92, 94, 96, 101), right=FALSE,
                              labels = c("a[0-80)", "b[80-85)", "c[85-90)","d[90-92)", "e[92-94)", "f[94-96)", "g[96-101]"))

# Define a color pallete corresponding to the punctuality ranges
pal = colorFactor(palette = c("#D7191C", "#F17C4A", "#FEC981","#FFFFC0", "#C4E687", "#77C35C", "#1A9641"), domain=locations$punctualityrange)

# Create the map object & add circle marker with label
leaflet() %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addCircleMarkers(data=locations,radius=2,color = ~ pal(punctualityrange), label = paste("percent on time=", round(locations$percent_on_time,digits=2)))

# second label:  "station=",locations$Station
----------------------------------------------
  

# VISU STOPS

# import data
data_path <- 'data/Output20/stat_delay_output.csv'
locations <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);

# create stp range to define the type as follows 
locations$stopsrange = cut(locations$Num_stops, 
                            breaks = c(0, 5, 10, 20, 40, 60, 90, 2300), right=FALSE,
                            labels = c("a[0-5)", "b[5-10)", "c[10-20)","d[20-40)", "e[40-60)", "f[60-90)", "g[90-2300]"))

# Define a color pallete corresponding to the stop ranges
pal = colorFactor(palette = c("#E3E3E3", "#B8B8B8", "#999999","#7A7A7A", "#636363", "#4D4D4D", "#0D0D0D"), domain=locations$stopsrange)

# Create the map object & add circle marker with label
leaflet() %>% addProviderTiles("Esri.WorldTopoMap", group = "Terrain")  %>% 
  addCircleMarkers(data=locations,radius=2,color = ~ pal(stopsrange), label = paste("percent on time=", round(locations$Num_stops,digits=2)))







