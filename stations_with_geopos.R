#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  stations_with_geopos.R
#   Author: Jonas Meyer
#   Date:   19.05.2020    meyj
#         
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Preliminaries
rm(list=ls(all=TRUE))     # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# load package
library(dplyr);

stations_with_geopos <- function(data, column_names){
    "This function takes as input a data frame of Swiss Public Transport Data 
    'Linien mit Betriebspunkten' and returns a data frame with unique Stations (no dublicates)
    
    input:  data: data frame from Swiss Public Transport Data
            column_names: Column Names for: station ID
                                            GeoPosition
                        
    output: data frame without dublicated stations and separated geoposition (lat, lon) "
    
    # rename column names
    data <- rename(data, 'BPUIC' = column_names[1], 'Position'= column_names[2]);
    # get index of unique values
    index <- match(unique(data$BPUIC), data$BPUIC);
    
    data <- data.frame('BPUIC' = data[index, 'BPUIC'], 'Position' = data[index,'Position']);
    
    # separate Position in to lat and lon
    data <- separate(data, Position, c("lat", "lon"), sep=",");
    
    # convert character to numeric
    options(digits = 10);
    data$lat <- as.numeric(data$lat);
    data$lon <- as.numeric(data$lon);
    
    return(data)
}


