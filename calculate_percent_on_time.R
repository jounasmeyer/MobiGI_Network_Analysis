#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  Calculate_percent_on_time.R
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

delays_per_station <- function(data, column_names){
    "This function takes as input a data frame of Swiss Public Transport Data 
    'Soll/Ist Vergleich Abfahrts-/Ankunftszeiten SBB (Vortag)' and returns a data frame
    with the statistics about trains on time
    
    input:  data: data frame from Swiss Public Transport Data
            column_names: Column Names for: station ID
                                            station Name
                                            Geo-Position
                                            Arrival delay
                                            Departure delay
    output: data frame with Number of train stops and delays, percentage of stops on time
            per Station "
    
    data <- rename(data, 'BPUIC' = column_names[1], 'HALTESTELLEN_NAME' = column_names[2], 'geopos' = column_names[3], 'ankunftsverspatung' = column_names[4], 'abfahrtsverspatung' = column_names[5])
    str(data)
    # get index of unique values of BP
    index <- match(unique(data$BPUIC), data$BPUIC);
    
    # Get statistics of kind of delay per BP
    stat <- data.frame(count(data, BPUIC, ankunftsverspatung, abfahrtsverspatung));
    
    # remove dateset with no delay
    stat_neg <- stat[which(stat$ankunftsverspatung=='false' & stat$abfahrtsverspatung=='false'),]
    stat_neg <- stat[which(stat$ankunftsverspatung=='false' & stat$abfahrtsverspatung=='false'),]
    stat <- anti_join(stat, stat_neg)
    
    # Aggregate all delays per BP
    stat_delay <- aggregate(n ~ BPUIC, data = stat, sum)
    colnames(stat_delay) <- c("BPUIC", "Num_delays")
    
    # unique values of bpuic
    t <- table(data$BPUIC)
    stat_bpuic <- data.frame(BPUIC=names(t),freq=as.numeric(t))
    stat_bpuic$BPUIC <- as.numeric(as.character(stat_bpuic$BPUIC));
    
    # Create new dataframe
    delays <- data.frame('BPUIC' = unique(data$BPUIC), 'Station' = data[index,'HALTESTELLEN_NAME'], 'Position'= data[index,'geopos']);
    
    # Join Dataframe
    stat_delay <- left_join(delays, stat_delay);
    stat_delay <- left_join(stat_delay, stat_bpuic);
    # Rename column-name
    colnames(stat_delay) <- c("BPUIC", "Station", "Position", "Num_delays", "Num_stops");
    
    # Change NA-values to 0 
    stat_delay[is.na(stat_delay)] <- 0;
    
    # Calculate percent on time
    percent_on_time <- (1 - stat_delay$Num_delays / stat_delay$Num_stops) * 100;
    
    # Combine Data to result dataframe
    stat_delay <- cbind(stat_delay, percent_on_time);
    
    return(stat_delay)
}
