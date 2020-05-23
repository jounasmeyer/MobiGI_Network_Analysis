#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  network_analysis_meyj.R
#   Author: Jonas Meyer
#   Date:   05.05.2020    meyj
#         
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Preliminaries
rm(list=ls(all=TRUE))     # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# Load Function from external files
source("datahandling.R")
library(dplyr);

# import data
data_path <- 'data/linie-mit-betriebspunkten_wgs84.csv';
data_geopos <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);
data_path <- 'Data/2019-05-05istdaten.csv';
data_delay <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);
data_delay <- data_delay[which(data_delay$PRODUKT_ID == 'Zug'),];


# Define col names vector
vec <- c('BPUIC','HALTESTELLEN_NAME', 'ankunftsverspatung', 'abfahrtsverspatung')

str(data_delay)


# Convert times in to POSIXct
data_delay$ABFAHRTSZEIT <-  as.POSIXct(data_delay$ABFAHRTSZEIT, format = '%d.%m.%Y %H:%M');
data_delay$AB_PROGNOSE <- as.POSIXct(data_delay$AB_PROGNOSE, format = '%d.%m.%Y %H:%M:%S');
data_delay$ANKUNFTSZEIT <-  as.POSIXct(data_delay$ANKUNFTSZEIT, format = '%d.%m.%Y %H:%M');
data_delay$AN_PROGNOSE <- as.POSIXct(data_delay$AN_PROGNOSE, format = '%d.%m.%Y %H:%M:%S');
str(data_delay)


index_dep <- which(difftime(data_delay$AB_PROGNOSE, data_delay$ABFAHRTSZEIT, units="mins") > 3 & data_delay$AB_PROGNOSE_STATUS == 'REAL');
index_arr <- which(difftime(data_delay$AN_PROGNOSE, data_delay$ANKUNFTSZEIT, units="mins") > 3 & data_delay$AN_PROGNOSE_STATUS == 'REAL');

abfahrtsverspatung <- replicate(nrow(data_delay), 'false');
ankunftsverspatung <- replicate(nrow(data_delay), 'false');

abfahrtsverspatung[index_dep] <- c('true');
#replace(abfahrtsverspatung, abfahrtsverspatung==FALSE, 'false');

ankunftsverspatung[index_arr] <- c('true');
#replace(ankunftsverspatung, ankunftsverspatung==FALSE, 'false');

data_delay <- cbind(data_delay,ankunftsverspatung, abfahrtsverspatung);
"""
data <- data_delay

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







"""
























# call function
stat_delay <- delays_per_station(data_delay, vec)
stat_geopos <- stations_with_geopos(data_geopos, c('BPUIC', 'geopos'))

# Join Dataframes
stat_delay_geopos <- left_join(stat_delay, stat_geopos, by = 'BPUIC');




data_S <- data_delay[which(data_delay$LINIEN_TEXT=='S3'),];
data_IC <- data_delay[which(data_delay$LINIEN_TEXT=='IC61' | data_delay$LINIEN_TEXT=='IC21' | data_delay$LINIEN_TEXT=='ICE' | data_delay$LINIEN_TEXT=='EC' & data_delay$LINIEN_TEXT=='IR26' | data_delay$LINIEN_TEXT=='IC6'),];
data_IR <- data_delay[which(data_delay$LINIEN_TEXT=='IR27'),];


data_S <- delays_per_station(data_S, vec)
data_IC <- delays_per_station(data_IC, vec)
data_IR <- delays_per_station(data_IR, vec)

# Export Dataset
write.csv2(data_S, 'Output/s_output.csv', row.names = F)
write.csv2(data_IC, 'Output/ic_output.csv', row.names = F)
write.csv2(data_IR, 'Output/ir_output.csv', row.names = F)


write.csv2(stat_delay_geopos, 'Output/stat_delay_output.csv', row.names = F)

