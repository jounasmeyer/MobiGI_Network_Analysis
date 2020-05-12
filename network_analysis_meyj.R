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
source("calculate_percent_on_time.R")

# import data
data_path <- 'Data/2020_05_05_ist-daten-sbb.csv';
data <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);

# Define col names vector
vec <- c('BPUIC','HALTESTELLEN_NAME', 'geopos', 'ankunftsverspatung', 'abfahrtsverspatung')
# call function
stat_delay <- delays_per_station(data, vec)

# Export Dataset
write.csv2(stat_delay, 'Output/stat_delay_output.csv', row.names = F)
