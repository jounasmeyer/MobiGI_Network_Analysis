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

# calculate delays
data_delay <- calculate_delays(data_delay)

# Define col names vector
vec <- c('BPUIC','HALTESTELLEN_NAME', 'ankunftsverspatung', 'abfahrtsverspatung')

# call function
stat_delay <- delays_per_station(data_delay, vec)
stat_geopos <- stations_with_geopos(data_geopos, c('BPUIC', 'geopos'))

# Join Dataframes
stat_delay_geopos <- left_join(stat_delay, stat_geopos, by = 'BPUIC');

# extract subsets
data_S <- data_delay[which(data_delay$LINIEN_TEXT=='S3'),];
data_IC <- data_delay[which(data_delay$LINIEN_TEXT=='IC61' | data_delay$LINIEN_TEXT=='IC21' | data_delay$LINIEN_TEXT=='ICE' | data_delay$LINIEN_TEXT=='EC' & data_delay$LINIEN_TEXT=='IR26' | data_delay$LINIEN_TEXT=='IC6'),];
data_IR <- data_delay[which(data_delay$LINIEN_TEXT=='IR27'),];

# summarise delays for subset
data_S <- delays_per_station(data_S, vec)
data_IC <- delays_per_station(data_IC, vec)
data_IR <- delays_per_station(data_IR, vec)

# Export datasets
write.csv2(data_S, 'Output/s_output.csv', row.names = F)
write.csv2(data_IC, 'Output/ic_output.csv', row.names = F)
write.csv2(data_IR, 'Output/ir_output.csv', row.names = F)
write.csv2(stat_delay_geopos, 'Output/stat_delay_output.csv', row.names = F)

