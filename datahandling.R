#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  datahandling.R
#   Author: Jonas Meyer
#   Date:   05.05.2020    meyj
#         
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Preliminaries
rm(list=ls(all=TRUE))     # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# load package
library(dplyr);
library(tidyr);

delays_per_station <- function(data, column_names){
    "This function takes as input a data frame of Swiss Public Transport Data 
    'Soll/Ist Vergleich Abfahrts-/Ankunftszeiten SBB (Vortag)' and returns a data frame
    with the statistics about trains on time
    
    input:  data: data frame from Swiss Public Transport Data
            column_names: Column Names for: station ID
                                            station Name
                                            Arrival delay
                                            Departure delay
    output: data frame with Number of train stops and delays, percentage of stops on time
            per Station "
    
    # rename column names
    data <- rename(data, 'BPUIC' = column_names[1], 'HALTESTELLEN_NAME' = column_names[2], 'ankunftsverspatung' = column_names[3], 'abfahrtsverspatung' = column_names[4]);
    
    # get index of unique values of BP
    index <- match(unique(data$BPUIC), data$BPUIC);
    
    # Get statistics of kind of delay per BP
    stat <- data.frame(count(data, BPUIC, ankunftsverspatung, abfahrtsverspatung));
    
    # remove dateset with no delay
    stat_neg <- stat[which(stat$ankunftsverspatung=='false' & stat$abfahrtsverspatung=='false'),];
    stat <- anti_join(stat, stat_neg);
    
    # Aggregate all delays per BP
    stat_delay <- aggregate(n ~ BPUIC, data = stat, sum);
    colnames(stat_delay) <- c("BPUIC", "Num_delays");
    
    # unique values of bpuic
    t <- table(data$BPUIC);
    stat_bpuic <- data.frame(BPUIC=names(t),freq=as.numeric(t));
    stat_bpuic$BPUIC <- as.numeric(as.character(stat_bpuic$BPUIC));
    
    # Create new dataframe
    delays <- data.frame('BPUIC' = unique(data$BPUIC), 'Station' = data[index,'HALTESTELLEN_NAME']);
    
    # Join Dataframe
    stat_delay <- left_join(delays, stat_delay);
    stat_delay <- left_join(stat_delay, stat_bpuic);
    # Rename column-name
    colnames(stat_delay) <- c("BPUIC", "Station", "Num_delays", "Num_stops");
    
    # Change NA-values to 0 
    stat_delay[is.na(stat_delay)] <- 0;
    
    # Calculate percent on time
    percent_on_time <- (1 - stat_delay$Num_delays / stat_delay$Num_stops) * 100;
    
    # Combine Data to result dataframe
    stat_delay <- cbind(stat_delay, percent_on_time);
    
    return(stat_delay);
}

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


calculate_delays <- function(data){
    "This function takes as input a data frame of Swiss Public Transport Data stored in Google Drive
    https://drive.google.com/drive/folders/1SVa68nJJRL3qgRSPKcXY7KuPN9MuHVhJ
    
    input:  data frame from Swiss Public Transport Data

    output: data frame with calculated delays as boolean"
    
    # Convert times in to POSIXct
    data$ABFAHRTSZEIT <-  as.POSIXct(data$ABFAHRTSZEIT, format = '%d.%m.%Y %H:%M');
    data$AB_PROGNOSE <- as.POSIXct(data$AB_PROGNOSE, format = '%d.%m.%Y %H:%M:%S');
    data$ANKUNFTSZEIT <-  as.POSIXct(data$ANKUNFTSZEIT, format = '%d.%m.%Y %H:%M');
    data$AN_PROGNOSE <- as.POSIXct(data$AN_PROGNOSE, format = '%d.%m.%Y %H:%M:%S');
    
    # find index of delays
    index_dep <- which(difftime(data$AB_PROGNOSE, data$ABFAHRTSZEIT, units="mins") > 3 & data$AB_PROGNOSE_STATUS == 'REAL');
    index_arr <- which(difftime(data$AN_PROGNOSE, data$ANKUNFTSZEIT, units="mins") > 3 & data$AN_PROGNOSE_STATUS == 'REAL');
    
    # create vector of length as data frame data_delay
    abfahrtsverspatung <- replicate(nrow(data), 'false');
    ankunftsverspatung <- replicate(nrow(data), 'false');
    
    abfahrtsverspatung[index_dep] <- c('true');
    ankunftsverspatung[index_arr] <- c('true');
    
    data <- cbind(data, ankunftsverspatung, abfahrtsverspatung);
    
    return(data)
}
