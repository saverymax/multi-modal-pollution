# This script creates a list of stations that fulfill criteria for training 
# time series models
# As of 2-23-2022, the criteria are that the data is the full sequence length,
# and contain 9999 or more non-missing values.

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(CADFtest)
library(vars)
library(imputeTS)
library(fGarch)
library(kableExtra)
library(forecast)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
setwd(user_dir)

# chem_names <- c("PM25", "SO2", "PM10", "NO2", "NOX", "CO")
# Only using these three, based off observations in plots from brussels_pollutants_ts_plots.R
chem_names <- c("PM25", "PM10", "NO2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

# This will be the length of the complete ts
complete_seq <- 10967

# Iterate through stations for each chemical and plot
# This loop is to look at the total length of the sequences and the number of missing values
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    print(chem)
    require(plyr)
    setwd(paste(user_dir, "data/eea_air/time_series_brux_", chem_id, sep=""))
    air_df <- ldply(list.files(), read.csv, header=T)
    # this package does not play nice with dplyr, so unload once done.
    detach("package:plyr", unload=T)
    
    require(dplyr)
    
    # get highest reading station
    air_summary <- air_df %>% group_by(AirQualityStation) %>% 
      summarise(avg_conc=mean(Concentration, na.rm=T), 
                sd_conc=sd(Concentration, na.rm=T), 
                length=n(), 
                min_date=min(DatetimeBegin), 
                max_date=max(DatetimeBegin)) %>% 
      arrange(desc(length), desc(avg_conc))
     
    output_file <- paste(user_dir, "data/top_stations_", chem, ".txt", sep="")
    # iterate by the stations with the most observations and avg concentration
    for (station_id in 1:nrow(air_summary)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% air_summary$AirQualityStation[station_id])
        station_name <- air_summary$AirQualityStation[station_id]
        
        # next fill in missing dates and run imputation
        # then select the date range that i want.
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        min(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        #station_subset <- highest_station %>% filter(datetime >= "2019-01-01" & datetime <= "2021-09-22")
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
        #station_subset <- highest_station %>% filter(datetime >= "2020-01-01" & datetime <= "2020-02-01")
        write(paste(station_name, nrow(station_subset), sum(!is.na(station_subset$Concentration)), sep=" "), file=output_file, append=T)
            
    }
}

# This next loop is the same as the above, but here I actually take the intersection of stations for the chemicals
# The stations being those that are complete and are not missing more than 1000 observations
station_list <- list(c(), c(), c())
names(station_list) <- names(chem_ids)
station_list
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    print(chem)
    require(plyr)
    setwd(paste(user_dir, "data/eea_air/time_series_brux_", chem_id, sep=""))
    air_df <- ldply(list.files(), read.csv, header=T)
    # this package does not play nice with dplyr, so unload once done.
    detach("package:plyr", unload=T)
    require(dplyr)
    
    # get highest reading station and longest stations 
    air_summary <- air_df %>% group_by(AirQualityStation) %>% 
      summarise(avg_conc=mean(Concentration, na.rm=T), 
                sd_conc=sd(Concentration, na.rm=T), 
                length=n(), 
                min_date=min(DatetimeBegin), 
                max_date=max(DatetimeBegin)) %>% 
      arrange(desc(length), desc(avg_conc))
     
    output_file <- paste(user_dir, "data/", "top_stations_", chem, ".txt", sep="")
    # iterate by the stations with the most observations and avg concentration
    for (station_id in 1:nrow(air_summary)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% air_summary$AirQualityStation[station_id])
        station_name <- air_summary$AirQualityStation[station_id]
        print(station_name)
        # next fill in missing dates and run imputation
        # then select the date range that i want.
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
        # Note all stations will have the complete sequence, ie, they will be missing days. I could complete them with 
        # the tidyr::complete method (see other scripts), but instead I am not going to include these edge cases in the analysis.
        # 9999 because that is the number for PM25, STA-BETB011.
        # There are a few other edge cases but I am using about no more than 1000 missing, so this is where I draw the line.
        if (nrow(station_subset) == complete_seq & sum(!is.na(station_subset$Concentration)) >= 9999){
            write(paste(station_name, nrow(station_subset), sum(!is.na(station_subset$Concentration)), sep=" "), file=output_file, append=T)
            station_list[[chem_index]] <- c(station_list[[chem_index]], station_name)
        }
    }
}
station_list

selected_stations <- Reduce(intersect, station_list)
selected_stations
station_file <- paste(user_dir, "data/selected_stations_for_training.txt")
write(selected_stations, file=station_file)
