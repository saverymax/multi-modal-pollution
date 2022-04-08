#########################################################
# Module for running VAR model for stations and chemicals
#########################################################

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(CADFtest)
library(vars)
library(imputeTS)
library(kableExtra)
library(forecast)

# Interesting sources on VECM vs VAR
# https://www.ifo.de/DocDL/cesifo1_wp1939.pdf
# https://aip.scitation.org/doi/pdf/10.1063/1.5016666

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/src/modelling"
setwd(user_dir)

# load covid data.
covid_df <- read_excel("d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/covid/covid19be.xlsx")
head(covid_df)
names(covid_df) <- tolower(names(covid_df))
head(covid_df)
# aggregate the classes.
covid_df %>% group_by()
covid_flanders <- covid_df %>% dplyr::filter(region=="Flanders") %>% 
    mutate(day=lubridate::ceiling_date(date, "day")) %>% 
    group_by(day) %>% summarise(total_cases=sum(cases))
covid_subset <- covid_flanders %>% dplyr::filter(day > "2020-03-01" & day <= "2021-06-02")
covid_subset
nrow(covid_subset)
min(covid_subset$day)
max(covid_subset$day)

# create covid ts
cov_ts <- ts(covid_subset$total_cases, frequency=7, start=c(2020, 9))
length(cov_ts)
#plot.ts(cov_ts)
png(filename="d:/asus_documents/ku_leuven/thesis/figures/covid_ts/cov_ts.png", width=1000)
p <- ggplot(covid_subset, aes(x=day, y=total_cases)) +
    geom_line() + 
    ggtitle("Daily COVID-19 cases, Flanders") +
    xlab("2020") +
    ylab("Daily cases")
p
dev.off()

plot.ts(cov_ts)

# test for non stationarity
cov_max_lag=round(sqrt(length(cov_ts)))
CADFtest(cov_ts, type= "drift", criterion= "BIC", max.lag.y=cov_max_lag)
CADFtest(diff(cov_ts), type= "drift", criterion= "BIC", max.lag.y=cov_max_lag)
# both are stationary

png(filename="d:/asus_documents/ku_leuven/courses/time_series/project/figures/cov_acf.png")
acf(cov_ts)
dev.off()
png(filename="d:/asus_documents/ku_leuven/courses/time_series/project/figures/cov_pacf.png")
pacf(cov_ts)
dev.off()
d_cov <- diff(cov_ts) 
acf(d_cov)
pacf(d_cov)
acf(diff(d_cov, 7))
pacf(diff(d_cov, 7))


# then load the traffic data.
traffic_df <- read_excel(
        path="d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/traffic/20210921-tunnels2019-sept2021.xlsx"
        ) 
names(traffic_df) <- tolower(names(traffic_df))
head(traffic_df)
nrow(traffic_df)
# first need to select the detector to use.
traffic_summary <- traffic_df %>% dplyr::group_by(detector) %>% 
  summarise(avg_tf=mean(volume, na.rm=T), sd_tf=sd(volume, na.rm=T), len=n()) %>% 
  arrange(desc(avg_tf))
traffic_summary
# using tun bel in because it has data for longest period with a high average
 detector_df <- traffic_df %>% dplyr::filter(detector=="Tun Bel IN")
# TODO: Pick measurement station or aggregation method
#detector_df <- traffic_df %>% dplyr::filter(detector=="Tun VP - A12")
nrow(detector_df)
detector_df$from <- ymd_hms(detector_df$from)
min(detector_df$from)
max(detector_df$from)
head(detector_df)
# this detector starts at 2019-01-01 00:00:00
# day duration counter https://www.timeanddate.com/date/duration.html?d1=01&m1=01&y1=2019&d2=&m2=&y2=&ti=on&
detector_subset <- detector_df %>% dplyr::filter(from > "2020-03-01 00:00:00" & from <= "2021-06-01")
min(detector_subset$from)
max(detector_subset$from)
nrow(detector_subset)

# fill in missing dates
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
detector_complete <- detector_subset %>% complete(from = seq(min(from), max(from), by="hour"))
nrow(detector_complete)
detector_complete[1:3, "from"]

# next do imputation. 
statsNA(as.matrix(detector_complete$volume))
# run imputations
# choice between linear, spline, and stineman interpolations
# need to round for some reason.
detector_complete$volume_inter <- round(na_interpolation(detector_complete$volume),1)
ggplot_na_imputations(detector_complete$volume, detector_complete$volume_inter)
statsNA(as.matrix(detector_complete$volume_inter))

# aggregate by day, but need to round up if staying in 2019
traffic_daily <- detector_complete %>% mutate(day=lubridate::ceiling_date(from, "day")) %>% group_by(day) %>% 
  summarise(daily_volume=mean(volume_inter)) 
traffic_daily
nrow(traffic_daily)
min(traffic_daily$day)
max(traffic_daily$day)
# create the ts
traffic_ts <- ts(traffic_daily$daily_volume, frequency=7, start=c(2020, 9))
stopifnot(length(traffic_ts)==458)

png(filename="d:/asus_documents/ku_leuven/thesis/figures/traffic_ts/traffic_ts.png", width=1000)
p <- ggplot(traffic_daily, aes(x=day, y=daily_volume)) +
    geom_line() + 
    ggtitle("Daily traffic volume, Brussels Tunnel") +
    xlab("2020") +
    ylab("Daily volume")
p
dev.off()

# test for non stationarity
tf_max_lag=round(sqrt(length(traffic_ts)))
CADFtest(traffic_ts, type= "drift", criterion= "BIC", max.lag.y=tf_max_lag)
CADFtest(diff(traffic_ts), type= "drift", criterion= "BIC", max.lag.y=tf_max_lag)
# in differences we have stationarity. in levels we do not
# for the air time series, it was stationary for both.

acf(traffic_ts)
pacf(traffic_ts)
d_traffic <- diff(traffic_ts) 
acf(d_traffic)
pacf(d_traffic)
acf(diff(d_traffic, 7))
pacf(diff(d_traffic))
# remember to correct for residuals when testing for cointegration.

# Testing for cointegration
length(cov_ts)
length(air_ts)
length(diff(traffic_ts))
# Set up dfs
var_df_cov <- data.frame(air_ts, traffic_ts, cov_ts)
var_df_no_cov <- data.frame(air_ts, traffic_ts)
var_df_no_air <- data.frame(traffic_ts, cov_ts)
# lag of 9
names(var_df_cov)<-c("air","traffic", "covid")
names(var_df_no_cov)<-c("air","traffic")
names(var_df_no_air)<-c("traffic","covid")
VARselect(var_df_cov,lag.max=10,type="const")
# order 8
VARselect(var_df_no_cov,lag.max=10,type="const")
# by sic we select order 8
VARselect(var_df_no_air,lag.max=10,type="const")
# order 9

# Testing cointegration
# This first test is the most important, because we have to check if the two 
# non-stationary series are cointegrated.
trace1 <- ca.jo(var_df_no_air,type="trace",K=9,ecdet="const",spec="transitory")
summary(trace1)
# The result supposts cointegration with 1 test equation.
trace2 <- ca.jo(var_df_cov,type="trace",K=8,ecdet="const",spec="transitory")
summary(trace2)
# When including that stationary air series, we get 2 test equations, though the second
# is rather on the boundary b/w signficant and not
# This test below is not necessary:
trace3 <- ca.jo(var_df_no_cov,type="trace",K=8,ecdet="const",spec="transitory")
summary(trace3)

#repeat the same procedure using johansen’s maximum eigenvalue test statistic.
# const is constant term in the long run relationshp
# transitory means to not include a deterministic trend in the test equation.
# we interpret this test as we did for the prvious test
maxeigen_test <- ca.jo(var_df_no_air,type="eigen",K=9,ecdet="const",spec="transitory")
summary(maxeigen_test)
# we see that we reject no cointegration at the 1% level

#############
# VAR model, for each chemical and station of interest
############

# Load selected stations as generated by station_select_for_mvts.R
station_file="d:/asus_documents/ku_leuven/thesis/data/selected_stations_for_training.txt"
selected_stations <- read.table(station_file)
names(selected_stations) <- c("station")
selected_stations
dim(selected_stations)

# Set up chemicals to focus on.
chem_names <- c("PM25", "PM10", "NO2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    if (chem != "NO2"){
        next
    }
    print(chem)
    require(plyr)
    setwd(paste("d:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/eea_air/time_series_brux_", chem_id, sep=""))
    air_df <- ldply(list.files(), read.csv, header=T)
    # this package does not play nice with dplyr, so unload once done.
    detach("package:plyr", unload=T)
    require(dplyr)
    predictions_df <- data.frame(observed=vector(mode='numeric',length=length(air_ts)))
    for (station_id in 1:nrow(selected_stations)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% selected_stations$station[station_id])
        print(station_id)
        #if (station_id > 1){
        #    break
        #}
        print(selected_stations$station[station_id])
        station_name <- selected_stations$station[station_id]
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        current_station$datetime <- ymd_hms(current_station$DatetimeBegin)
        # need to do le and ge because of how the == is computed.
        station_subset <- current_station %>% dplyr::filter(datetime >= "2020-03-01" & datetime <= "2021-06-01")
        station_complete <- station_subset %>% tidyr::complete(datetime = seq(min(datetime), max(datetime), by="hour"))
        # this now contains a complete time series for the selected dates
        # because of creating new time indices as well as accounting for any missing data, it is necessary to do imputation.
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
 
        # Going in differences for the non-stationary series
        # Need to remove first measurement from stationary series
        var_df_cov <- data.frame(air_ts[2:length(air_ts)], diff(traffic_ts), diff(cov_ts))
        var_df_no_cov <- data.frame(air_ts[2:length(air_ts)], diff(traffic_ts))
        names(var_df_cov)<-c("air","traffic", "covid")
        names(var_df_no_cov)<-c("air","traffic")
        # there may be cases where using nonstationary series in var is ok.
        fit_var1 <- VAR(var_df_no_cov,type="const",p=8)
        fit_var2 <- VAR(var_df_cov,type="const",p=8)
        summary(fit_var1)
        summary(fit_var2)
        
        # checking the residuals
        var2_residuals<-resid(fit_var2)
        acf(var2_residuals[,1])
        acf(var2_residuals[,2])
        acf(var2_residuals[,3])
        ccf(var2_residuals[,1],var2_residuals[,2])
        ccf(var2_residuals[,1],var2_residuals[,3])
        ccf(var2_residuals[,2],var2_residuals[,3])
        
        # we then examine the impulse function
        #irf_var<-irf(fit_var2,ortho=F,boot=T)
        #plot(irf_var)
        
        # Generating predictions
        H <- c(1, 5, 10)
        s <- round(0.75*length(air_ts))
        dm_tests <- matrix(nrow=length(H), ncol=3)
        oos_errors <- matrix(nrow=3, ncol=5)
        # Add observed values to df for easy error computation in evaluation script
        predictions_df$observed <- air_daily$daily_concentration
        for (i in 1:length(H)){
            h <- H[i]
            print(h)
            error_cov <- c()
            error_nocov <- c()
            pred_id_cov <- paste(chem, "_", station_name, "_horizon-", h, "_cov", sep="")
            pred_id_no_cov <- paste(chem, "_", station_name, "_horizon-", h, "_nocov", sep="")
            predictions_cov_vec <- vector(mode="numeric", length=length(air_ts))
            predictions_no_cov_vec <- vector(mode="numeric", length=length(air_ts))
            for (k in s:(length(air_ts)-h)){
                var_df_cov_sub <- data.frame(air_ts[1:k], diff(traffic_ts)[1:k], diff(cov_ts)[1:k])
                var_df_no_cov_sub <- data.frame(air_ts[1:k], diff(traffic_ts)[1:k])
                names(var_df_cov_sub)<-c("air","traffic", "covid")
                names(var_df_no_cov_sub)<-c("air","traffic")
                fit_var_cov <- VAR(var_df_cov_sub,type="const",p=8)
                fit_var_nocov <- VAR(var_df_no_cov_sub,type="const",p=8)
                # predict h step ahead
                predict_cov_out <- predict(fit_var_cov,n.ahead=h)
                predict_cov <- predict_cov_out[[1]][1]$air[h]
                predict_nocov_out <- predict(fit_var_nocov,n.ahead=h)
                predict_nocov <- predict_nocov_out[[1]][1]$air[h]
                # subtract the predicted value predict.h from the observed value, y[i+h]
                error_cov <- c(error_cov,air_ts[k+h]-predict_cov)
                error_nocov <- c(error_nocov,air_ts[k+h]-predict_nocov)
                # Then prep for output
                predictions_cov_vec[k+h] <- predict_cov
                predictions_no_cov_vec[k+h] <- predict_nocov
            }
            predictions_df[pred_id_cov] <- predictions_cov_vec
            predictions_df[pred_id_no_cov] <- predictions_no_cov_vec
            cov_mape <- mean(abs(error_cov)/air_ts[(s+h):length(air_ts)])
            nocov_mape <- mean(abs(error_nocov)/air_ts[(s+h):length(air_ts)])
            cov_rmse <- sqrt(mean(error_cov^2))
            nocov_rmse <- sqrt(mean(error_nocov^2))
            oos_errors[i, 1] <- h
            oos_errors[i, 2] <- cov_mape
            oos_errors[i, 3] <- cov_rmse
            oos_errors[i, 4] <- nocov_mape
            oos_errors[i, 5] <- nocov_rmse
            dm_tests[i, 1] <- h
            dm_tests[i, 2] <- dm.test(error_cov, error_nocov,h=h,power=1)$p.value[[1]]
            dm_tests[i, 3] <- dm.test(error_cov, error_nocov,h=h,power=2)$p.value[[1]]
        }
        oos_errors
        dm_tests
        oos_df <- as.data.frame(oos_errors)
        dm_tests
        dm_df <- as.data.frame(dm_tests)
        colnames(oos_df) <- c("Horizon", "W/ COVID MAPE", "W/COVID RMSE", "W/out COVID MAPE", "W/out COVID RMSE")
        colnames(dm_df) <- c("Horizon", "MAE", "MSE")
        print(kbl(oos_df, booktabs = T, format="latex"))
        print(kbl(dm_df, booktabs = T, format="latex"))
        
    }
    setwd(user_dir)
    write.csv(predictions_df, paste("../../data/model_output/arima/", chem, "_VAR_forecasts.csv", sep=""), row.names = FALSE)
}
        
#########
# This code was initlaly intended for the time series course. It may not be necessary for here
#########
# The analysis is then repeated for October to the end of the year, to account for the period
# of the most intense covid
# First traffic
detector_subset <- detector_df %>% dplyr::filter(from > "2020-10-01 00:00:00" & from <= "2021-01-01")
detector_complete <- detector_subset %>% complete(from = seq(min(from), max(from), by="hour"))
nrow(detector_complete)
detector_complete$volume_inter <- round(na_interpolation(detector_complete$volume),1)
# aggregate by day, but need to round up if staying in 2019
traffic_daily <- detector_complete %>% mutate(day=lubridate::ceiling_date(from, "day")) %>% group_by(day) %>% 
  summarise(daily_volume=mean(volume_inter)) 
traffic_daily
nrow(traffic_daily)
min(traffic_daily$day)
max(traffic_daily$day)
traffic_ts <- ts(traffic_daily$daily_volume, frequency=7, start=c(2020, 39))
# Covid
covid_subset <- covid_flanders %>% dplyr::filter(day > "2020-10-01" & day <= "2021-01-02")
covid_subset
nrow(covid_subset)
min(covid_subset$day)
max(covid_subset$day)
cov_ts <- ts(covid_subset$total_cases, frequency=7, start=c(2020, 39))
# And NO2
station_subset <- highest_station %>% dplyr::filter(datetime >= "2020-10-01" & datetime <= "2021-01-01")
station_complete <- station_subset %>% tidyr::complete(datetime = seq(min(datetime), max(datetime), by="hour"))
nrow(station_complete)
station_complete$concentration_inter <- round(na_interpolation(station_complete$Concentration),1)
air_daily <- station_complete %>% mutate(day=lubridate::ceiling_date(datetime, "day")) %>% group_by(day) %>% 
  summarise(daily_concentration=mean(concentration_inter)) 
air_daily
# create the ts
air_ts <- ts(air_daily$daily_concentration, frequency=7, start=c(2020, 39))

ts.plot(air_ts)
ts.plot(cov_ts)
ts.plot(traffic_ts)

# Then reselect lags and remodel
length(cov_ts)
length(air_ts)
length(traffic_ts)
# Set up dfs
var_df_cov <- data.frame(air_ts, traffic_ts, cov_ts)
var_df_no_cov <- data.frame(air_ts, traffic_ts)
names(var_df_cov)<-c("air","traffic", "covid")
names(var_df_no_cov)<-c("air","traffic")
VARselect(var_df_cov,lag.max=10,type="none")
VARselect(var_df_no_cov,lag.max=10,type="none")
# both still order 8

# REgenerating predictions
H <- c(1, 5, 10)
s <- round(0.75*length(air_ts))
dm_tests <- matrix(nrow=length(H), ncol=3)
oos_errors <- matrix(nrow=3, ncol=5)
for (i in 1:length(H)){
    h <- H[i]
    print(h)
    error_cov <- c()
    error_nocov <- c()
    for (k in s:(length(air_ts)-h)){
        var_df_cov_sub <- data.frame(air_ts[1:k], diff(traffic_ts)[1:k], diff(cov_ts)[1:k])
        var_df_no_cov_sub <- data.frame(air_ts[1:k], diff(traffic_ts)[1:k])
        names(var_df_cov_sub)<-c("air","traffic", "covid")
        names(var_df_no_cov_sub)<-c("air","traffic")
        fit_var_cov <- VAR(var_df_cov_sub,type="const",p=8)
        fit_var_nocov <- VAR(var_df_no_cov_sub,type="const",p=8)
        # predict h step ahead
        predict_cov_out <- predict(fit_var_cov,n.ahead=h)
        predict_cov <- predict_cov_out[[1]][1]$air[h]
        predict_nocov_out <- predict(fit_var_nocov,n.ahead=h)
        predict_nocov <- predict_nocov_out[[1]][1]$air[h]
        # subtract the predicted value predict.h from the observed value, y[i+h]
        error_cov <- c(error_cov,air_ts[k+h]-predict_cov)
        error_nocov <- c(error_nocov,air_ts[k+h]-predict_nocov)
    }
    cov_mape <- mean(abs(error_cov)/air_ts[(s+h):length(air_ts)])
    nocov_mape <- mean(abs(error_nocov)/air_ts[(s+h):length(air_ts)])
    cov_rmse <- sqrt(mean(error_cov^2))
    nocov_rmse <- sqrt(mean(error_nocov^2))
    oos_errors[i, 1] <- h
    oos_errors[i, 2] <- cov_mape
    oos_errors[i, 3] <- cov_rmse
    oos_errors[i, 4] <- nocov_mape
    oos_errors[i, 5] <- nocov_rmse
    dm_tests[i, 1] <- h
    dm_tests[i, 2] <- dm.test(error_cov, error_nocov,h=h,power=1)$p.value[[1]]
    dm_tests[i, 3] <- dm.test(error_cov, error_nocov,h=h,power=2)$p.value[[1]]
}
oos_errors
dm_tests
oos_df <- as.data.frame(oos_errors)
dm_tests
dm_df <- as.data.frame(dm_tests)
colnames(oos_df) <- c("Horizon", "W/ COVID MAPE", "W/COVID RMSE", "W/out COVID MAPE", "W/out COVID RMSE")
colnames(dm_df) <- c("Horizon", "MAE", "MSE")
kbl(oos_df, booktabs = T, format="latex")
kbl(dm_df, booktabs = T, format="latex")

