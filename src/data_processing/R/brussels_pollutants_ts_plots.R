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
# Note that only PM10, PM25, and NO2 are used for thesis work.

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
setwd(user_dir)

chem_names <- c("PM25", "PM10", "NO2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

station_file="data/selected_stations_for_training.txt"
selected_stations <- read.table(station_file)
names(selected_stations) <- c("station")

# Lockdown times
lockdown_regions <- data.frame(xmin=c(as.POSIXlt("2020-03-18"), as.POSIXlt("2020-10-28")), 
                               xmax=c(as.POSIXlt("2020-05-06"), as.POSIXlt("2021-05-08")), 
                               ymin=-Inf, ymax=Inf)

# Iterate through stations for each chemical and plot
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
    
    for (station_id in 1:nrow(selected_stations)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% selected_stations$station[station_id])
        print(station_id)
        print(selected_stations$station[station_id])
        station_name <- selected_stations$station[station_id]
        # next fill in missing dates and run imputation
        # then select the date range that i want.
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        min(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
        # Can only show stations for which there is data and for which is not completely no
        #if (nrow(station_subset) > 0 & sum(!is.na(station_subset$Concentration)) > 3){
        if (nrow(station_subset) > 0 & sum(!is.na(station_subset$Concentration)) > 3){
            
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
            
            png(filename=paste(user_dir, "/data/figures/brussels_air_ts/", chem, "_", station_name, "_", station_id, ".png", sep=""), width=700)
            #ts.plot(air_ts)
            p <- ggplot(air_daily, aes(x=day, y=daily_concentration)) +
                geom_line() + 
                ggtitle(paste("Daily average, ", chem, ", ", station_name, sep="")) +
                xlab("Date") +
                ylab("Avg. Concentration") + theme_bw(base_size=22) + 
                theme(axis.text.y = element_text(size = 20), 
                      axis.text.x = element_text(size = 20, angle=45, vjust = .6)) +
                geom_rect(data=lockdown_regions, alpha=.3, inherit.aes=FALSE, 
                  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="steelblue3") -> p
            print(p)
            dev.off()
        }
    }
}

