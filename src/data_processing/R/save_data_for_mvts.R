# Saving data in format for MVTS transformer package.
# mvts_transformer expects a .ts file, and will load it into pandas.
# However, this script saves the data directly into a csv that can be loaded 
# into pandas in the same format as expected by MVTS.
# https://www.sktime.org/en/stable/examples/loading_data.html#ts_files

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(imputeTS)
library(kableExtra)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/src/data_processing/R"
setwd(user_dir)

chem_names <- c("PM25", "PM10", "NO2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

# Load selected stations from station_select_for_mvts.R
station_file="d:/asus_documents/ku_leuven/thesis/data/selected_stations_for_training.txt"
selected_stations <- read.table(station_file)
names(selected_stations) <- c("station")
selected_stations
dim(selected_stations)

# Iterate through stations for each chemical 
n_days <- 458
n_days*nrow(selected_stations)
ts_matrix <- matrix(data=NA, nrow=n_days*nrow(selected_stations), ncol=length(chem_ids)+1)
dim(ts_matrix)
# Number of days of for datetime >= "2020-03-01" & datetime <= "2021-06-01"
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    #if (chem_index > 1){
        #break
    #}
    chem <- names(chem_ids)[chem_index]
    print(chem)
    require(plyr)
    setwd(paste("d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/eea_air/time_series_brux_", chem_id, sep=""))
    air_df <- ldply(list.files(), read.csv, header=T)
    # this package does not play nice with dplyr, so unload once done.
    detach("package:plyr", unload=T)
    require(dplyr)
    
    # Iterate through the list of stations
    row_index <- 1
    for (station_id in 1:nrow(selected_stations)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% selected_stations$station[station_id])
        print(station_id)
        print(selected_stations$station[station_id])
        station_name <- selected_stations$station[station_id]
        
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
            
        station_complete <- station_subset %>% tidyr::complete(datetime = seq(min(datetime), max(datetime), by="hour"))
        # this now contains a complete time series for the selected dates
        # because of creating new time indices, it is necessary to do imputation.
        # using imputets, for the univariate case.
        # https://cran.r-project.org/web/packages/imputets/vignettes/imputets-time-series-missing-value-imputation-in-r.pdf
        # choice between linear, spline, and stineman interpolations
        # need to round for some reason.
        #station_complete$concentration_inter <- round(na_interpolation(station_complete$Concentration),1)
        station_complete$concentration_inter <- round(na_kalman(station_complete$Concentration, smooth = T),1)
        #ggplot_na_imputations(station_complete$Concentration, station_complete$concentration_inter)
        #statsNA(as.matrix(station_complete$concentration_inter))
        # aggregate by day, but need to round up if staying in 2019
        air_daily <- station_complete %>% mutate(day=lubridate::ceiling_date(datetime, "day")) %>% group_by(day) %>% 
          summarise(daily_concentration=mean(concentration_inter)) 
        # create the ts
        air_ts <- ts(air_daily$daily_concentration, frequency=7, start=c(2020, 9))
        ts_matrix[row_index:(row_index+n_days-1), chem_index] <- air_ts
        # Only need to do this once
        if (chem_index == 1){
            ts_matrix[row_index:(row_index+n_days-1), length(chem_ids)+1] <- station_name
        }   
        row_index <- row_index + n_days
        print(row_index)
        #if (station_id > 2){
        #    break
        #}
    }
}
# Assert any nas
stopifnot(sum(is.na(ts_matrix))==0)
dim(ts_matrix)
df_ts <- as.data.frame(ts_matrix)
names(df_ts) <- c(chem_names, "station")
df_ts
dim(df_ts)

# Next get traffic and COVID data.
covid_df <- read_excel("d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/covid/covid19be.xlsx")
names(covid_df) <- tolower(names(covid_df))
head(covid_df)
# aggregate the classes.
covid_df %>% group_by()
covid_flanders <- covid_df %>% dplyr::filter(region=="Flanders") %>% 
    mutate(day=lubridate::ceiling_date(date, "day")) %>% 
    group_by(day) %>% summarise(total_cases=sum(cases))
covid_flanders
covid_subset <- covid_flanders %>% dplyr::filter(day > "2020-03-01" & day <= "2021-06-02")
covid_subset
nrow(covid_subset)
min(covid_subset$day)
max(covid_subset$day)
# there are no missing dates or nas, so no imputation necessary
# Add COVID data to the df for chemicals
rep_cases <- rep(covid_subset$total_cases, nrow(selected_stations))
rep_cases
stopifnot(nrow(df_ts)==length(rep_cases))
df_ts$COVID <- rep_cases
dim(df_ts)

# then load the traffic data.
# https://en.wikipedia.org/wiki/List_of_tunnels_in_Belgium
# Can use this to find the abbreviations and locations of tunnels
# https://data.mobility.brussels/mobigis/?x=485351&y=6593040&zoom=12&baselayer=urbis_grey&layers=traffic_live_geom%3BTunnels%3B
# Tunnels in entry points of brussels:
# Was going to include "Tun Terv - Centre" but not enough data
tunnels <- c("Tun VP - A12", "Tun Del - Parking",  "Tun Montg - Cambre", "Tun Ste OUT - Centre et Bas - Cambre", "Tun Lou IN - Bas - Midi et Cambre") 

traffic_df <- read_excel(
        path="d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/traffic/20210921-tunnels2019-sept2021.xlsx"
        ) 
names(traffic_df) <- tolower(names(traffic_df))
# first need to select the detector to use.
traffic_summary <- traffic_df %>% dplyr::group_by(detector) %>% 
  summarise(avg_tf=mean(volume, na.rm=T), sd_tf=sd(volume, na.rm=T), len=n()) %>% 
  arrange(desc(avg_tf))
traffic_summary$detector
# using tun bel in because it has data for longest period with a high average
#detector_df <- traffic_df %>% dplyr::filter(detector=="Tun Bel IN")
detector_df <- traffic_df %>% dplyr::filter(detector %in% tunnels) 
dim(detector_df)
head(detector_df)
detector_df$from <- ymd_hms(detector_df$from)
min(detector_df$from)
max(detector_df$from)
# this detector starts at 2019-01-01 00:00:00
# day duration counter https://www.timeanddate.com/date/duration.html?d1=01&m1=01&y1=2019&d2=&m2=&y2=&ti=on&
detector_subset <- detector_df %>% dplyr::filter(from > "2020-03-01 00:00:00" & from <= "2021-06-01")
min(detector_subset$from)
max(detector_subset$from)
nrow(detector_subset)

# fill in missing dates
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
detector_complete <- detector_subset %>% group_by(detector) %>% tidyr::complete(from = seq(min(from), max(from), by="hour"))
detector_complete
min(detector_complete$from)
max(detector_complete$from)
nrow(detector_complete)

# next do imputation. variations options are available.
# https://cran.r-project.org/web/packages/amelia/index.html
# http://schd.ws/hosted_files/user2017/e5/user2017_steffen_moritz.pdf
# https://cran.r-project.org/web/packages/imputets/
# https://stats.stackexchange.com/questions/261271/imputation-methods-for-time-series-data
# https://stats.stackexchange.com/questions/261271/imputation-methods-for-time-series-data

statsNA(as.matrix(detector_complete$volume))
# run imputations
# choice between linear, spline, and stineman interpolations
#detector_complete$volume_inter <- round(na_interpolation(detector_complete$volume),1)
# Kalman filtering looks really bad in this data
# detector_complete$volume_inter <- round(na_kalman(detector_complete$volume, smooth=T),1)
#ggplot_na_imputations(detector_complete$volume, detector_complete$volume_inter)
#statsNA(as.matrix(detector_complete$volume_inter))

detector_complete <- detector_complete %>% 
    group_by(detector) %>% 
    mutate(volume_inter=round(na_interpolation(volume), 1))

detector_complete
statsNA(as.matrix(detector_complete$volume_inter)) 

# aggregate by day, but need to round up if staying in 2019
traffic_daily <- detector_complete %>% group_by(detector) %>% mutate(day=lubridate::ceiling_date(from, "day")) %>% group_by(day, .add=T) %>% 
  summarise(daily_volume=mean(volume_inter)) 

View(traffic_daily)

traffic_daily %>% group_by(detector) %>% summarise(n = n(), min=min(day), max=max(day))

# create the ts
# First need to transpose
traffic_transpose <- tidyr::pivot_wider(traffic_daily, names_from=detector, values_from = daily_volume)
traffic_transpose
rep_traffic <- do.call(rbind, replicate(nrow(selected_stations), as.matrix(traffic_transpose), simplify=FALSE))
nrow(rep_traffic)
head(rep_traffic)
# Nice function but it only repeats the rows in an adjacent way, not in an rbind dataframe way...
# rep_traffic <- traffic_transpose %>% uncount(nrow(selected_stations)) 
# Way of doing it when there was just one measuring station
#rep_traffic <- rep(traffic_daily$daily_volume, nrow(selected_stations)) 
#df_ts$traffic_vol <- rep_traffic
stopifnot(nrow(df_ts)==nrow(rep_traffic))
df_join <- cbind(df_ts, rep_traffic)
df_join$time <- rep(traffic_transpose$day, nrow(selected_stations))
View(df_join)
# Funny little bit of code to drop day.
df_join <- df_join[!names(df_join) %in% c("day")]

# As of 4-7-2022, I want to split the time series into smaller series. 
# I need to create a new id that is indexed by the air measuring station and the time subsets I want
split_time <- vector(length=nrow(df_join))
sub_ids <- vector(length=nrow(df_join))
length(sub_ids)
time_cnt <- 0
sub_id <- 1
seq_len <- 30
# Splits into 16 series if seq len 30.
current_station = df_join[1, 'station']
for (i in 1:nrow(df_join)){
    time_cnt <- time_cnt + 1
    if (time_cnt == seq_len + 1){
        time_cnt <- 1
        sub_id <- sub_id + 1 
    }
    if (df_join[i, 'station'] != current_station){
        # When we change stations, reset time and sub ids.
        sub_id <- 1
        time_cnt <- 1
    }
    current_station <- df_join[i, 'station']    
    new_id <- paste(current_station, "_", sub_id, sep = "")
    sub_ids[i] <- new_id
    split_time[i] <- time_cnt
}
sub_ids
split_time
df_join$station_subset <- sub_ids
df_join$split_time <- split_time
View(df_join)
names(df_join) <- gsub("__", "_", gsub(" ", "_", gsub("-", "", tolower(names(df_join)))))


# Note that this has to be manually checked based on the specified seq_len
# because the date is chosen based on the sub_id so that the train and test
# sets will contain complete series. We don't want to split so that 
# some series are divided in half.
setwd(user_dir)
train_subset <- df_join %>% dplyr::filter(time < "2021-02-24")
test_subset <- df_join %>% dplyr::filter(time >= "2021-02-24")
# Do a quick check
require(stringr)
min_id <- 13
match_cnt <- 0
for (i in 1:nrow(test_subset)){
    sub_id <- test_subset[i, 'station_subset']
    current_id <- str_split(sub_id, "_")[[1]][2]
    stopifnot(as.numeric(current_id) >= min_id)
    if (as.numeric(current_id) == min_id){
        match_cnt <- match_cnt + 1
    }
}
# Number of stations times 1 times length of that series
stopifnot(match_cnt == 41*30)
# Then trainset
min_id <- 13
match_cnt <- 0
for (i in 1:nrow(train_subset)){
    sub_id <- train_subset[i, 'station_subset']
    current_id <- str_split(sub_id, "_")[[1]][2]
    stopifnot(as.numeric(current_id) < min_id)
    if (as.numeric(current_id) == min_id - 1){
        match_cnt <- match_cnt + 1
    }
}
# Number of stations times 1 times length of that series
stopifnot(match_cnt == 41*30)
# Should check out
write_csv(df_join, file="../../../data/mvts/air_quality_bxl_all.csv")
write_csv(train_subset, file="../../../data/mvts/air_quality_bxl_train.csv")
write_csv(test_subset, file="../../../data/mvts/air_quality_bxl_test.csv")
