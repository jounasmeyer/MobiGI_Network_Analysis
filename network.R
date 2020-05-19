#--------------------------------------------------------------------------------------------------------------------------------------------------
#   MobiGI Network Analysis 
#   
#   Title:  network.R
#   Author: Jonas Meyer
#   Date:   12.05.2020    meyj
#         
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Preliminaries
rm(list=ls(all=TRUE))     # clean the environment
options(scipen=6)         # display digits properly!! not the scientific version
options(digits.secs=6)    # use milliseconds in Date/Time data types
options(warning=FALSE)    # don't show warnings

# Load Function from external files
source("calculate_percent_on_time.R")



point1 <- 8506000;

data_path <- 'Data/linie-mit-betriebspunkten_wgs84.csv';
data <- read.csv(data_path, sep = ';', encoding = 'UTF-8', stringsAsFactors = FALSE);

lines <- aggregate(BPUIC ~ LINIE, data, c)
station <- aggregate(LINIE ~ BPUIC, data, c)



