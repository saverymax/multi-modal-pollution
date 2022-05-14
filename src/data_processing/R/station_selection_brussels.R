# Script to select stations in brussels.

library(tidyverse)
library(lubridate)
library(readxl)

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

station_list <- list(c(), c(), c())
names(station_list) <- names(chem_ids)
station_list
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    print(chem)
    require(plyr)
    setwd(paste(user_dir, "/data/eea_air/time_series_brux_", chem_id, sep=""))
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
        station_list[[chem_index]] <- c(station_list[[chem_index]], station_name)
    }
}
station_list