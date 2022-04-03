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


# This script generates TS plots for stations of interest for each chemical of interest in Brussels
# Generate the plots for each city for each chemical
chem_names <- c("PM25", "SO2", "PM10", "NO2", "NOX", "CO")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 1
chem_ids[[3]] <- 5
chem_ids[[4]] <- 8
chem_ids[[5]] <- 9
chem_ids[[6]] <- 10

# Iterate through stations for each chemical and plot
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    print(chem)
    require(plyr)
    setwd(paste("d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/eea_air/time_series_brux_", chem_id, sep=""))
    air_df <- ldply(list.files(), read.csv, header=T)
    # this package does not play nice with dplyr, so unload once done.
    detach("package:plyr", unload=T)
    
    require(dplyr)
    # check memory
    
    # get highest reading station
    air_summary <- air_df %>% group_by(AirQualityStation) %>% 
      summarise(avg_conc=mean(Concentration, na.rm=T), 
                sd_conc=sd(Concentration, na.rm=T), 
                length=n(), 
                min_date=min(DatetimeBegin), 
                max_date=max(DatetimeBegin)) %>% 
      arrange(desc(length), desc(avg_conc))
     
    # get the stations with the most observations 
    # Iterate through by rank based on avg concentration
    for (station_id in 1:nrow(air_summary)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% air_summary$AirQualityStation[station_id])
        station_name <- air_summary$AirQualityStation[station_id]
        print(station_name)
        
        # next fill in missing dates and run imputation
        # then select the date range that i want.
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        min(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        #station_subset <- highest_station %>% filter(datetime >= "2019-01-01" & datetime <= "2021-09-22")
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
        #station_subset <- highest_station %>% filter(datetime >= "2020-01-01" & datetime <= "2020-02-01")
        if (nrow(station_subset) > 0 & sum(!is.na(station_subset$Concentration)) > 2){
            
            station_complete <- station_subset %>% tidyr::complete(datetime = seq(min(datetime), max(datetime), by="hour"))
            # this now contains a complete time series for 2019-01-01 through 2021-09-21
            # because of creating new time indices, it is necessary to do imputation.
            # using imputets, for the univariate case.
            # https://cran.r-project.org/web/packages/imputets/vignettes/imputets-time-series-missing-value-imputation-in-r.pdf
            # choice between linear, spline, and stineman interpolations
            # need to round for some reason.
            #station_complete$concentration_inter <- round(na_interpolation(station_complete$Concentration),1)
            station_complete$concentration_inter <- round(na_kalman(station_complete$Concentration, smooth = T),1)
            
            # aggregate by day, but need to round up if staying in 2019
            air_daily <- station_complete %>% mutate(day=lubridate::ceiling_date(datetime, "day")) %>% group_by(day) %>% 
              summarise(daily_concentration=mean(concentration_inter)) 
            nrow(air_daily)
            min(air_daily$day)
            max(air_daily$day)
            # create the ts
            air_ts <- ts(air_daily$daily_concentration, frequency=7, start=c(2020, 9))
            
            png(filename=paste("d:/asus_documents/ku_leuven/thesis/figures/brussels_ts/", chem, "_", station_name, "_", station_id, ".png", sep=""), width=1000)
            #ts.plot(air_ts)
            p <- ggplot(air_daily, aes(x=day, y=daily_concentration)) +
                geom_line() + 
                ggtitle(paste("Daily average, ", chem, ", ", station_name, sep="")) +
                xlab("October 2020-June 2021") +
                ylab("daily concentration")
            print(p)
            dev.off()
        }
    }
}

