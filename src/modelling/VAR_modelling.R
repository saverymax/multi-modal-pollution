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
library(scales)

# Interesting sources on VECM vs VAR
# https://www.ifo.de/DocDL/cesifo1_wp1939.pdf
# https://aip.scitation.org/doi/pdf/10.1063/1.5016666

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
setwd(user_dir)

# load covid data.
covid_df <- read_excel("data/covid/covid19be.xlsx")
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

png(filename="data/figures/covid_ts/levels_Flanders_ts.png", width=700)
p <- ggplot(covid_subset, aes(x=day, y=total_cases)) +
    geom_line() + 
    ggtitle("Daily COVID-19 cases, Flanders") +
    xlab("Date") +
    ylab("Daily cases") + 
    theme_bw() +
    theme(title = element_text(size=24), 
                       axis.text.y = element_text(size = 22),
                       axis.text.x = element_text(size = 22, angle=45, vjust = .6))
p        
dev.off()

diff_df <- data.frame(y=diff(covid_subset$total_cases))
diff_df$x <- covid_subset$day[2:nrow(covid_subset)]
png(filename="data/figures/covid_ts/diff_Flanders_ts.png", width=700)
p <- ggplot(diff_df, aes(x=x, y=y)) +
    geom_line() + 
    ggtitle("Daily COVID-19 cases in differences, Flanders") +
    xlab("Date") +
    ylab("Daily cases") + 
    theme_bw() +
    theme(title = element_text(size=24), 
                       axis.text.y = element_text(size = 22),
                       axis.text.x = element_text(size = 22, angle=45, vjust = .6))
p        
dev.off()

png(filename="data/figures/covid_ts/cov_acf.png")
acf(cov_ts, main="COVID autocorrelation")
dev.off()
png(filename="data/figures/covid_ts/cov_pacf.png")
pacf(cov_ts, main="COVID partial autocorrelation")
dev.off()
d_cov <- diff(cov_ts) 
png(filename="data/figures/covid_ts/diff_cov_acf.png")
acf(d_cov, main="COVID autocorrelation after differencing")
dev.off()
png(filename="data/figures/covid_ts/diff_cov_pacf.png")
pacf(d_cov, main="COVID partial autocorrelation after differencing")
dev.off()
acf(diff(d_cov, 7))
pacf(diff(d_cov, 7))

# then load the traffic data.
traffic_df <- read_excel(
        path="data/traffic/20210921-Tunnels2019-sept2021.xlsx"
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
detector_complete$volume_inter <- na_interpolation(detector_complete$volume)
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

png(filename="data/figures/traffic_ts/traffic_acf.png")
acf(traffic_ts, main="Traffic autocorrelation")
dev.off()
png(filename="data/figures/traffic_ts/traffic_pacf.png")
pacf(traffic_ts, main="Traffic partial autocorrelation")
dev.off()
png(filename="data/figures/traffic_ts/diff_traffic_acf.png")
acf(diff(traffic_ts), main="Traffic autocorrelation after differencing")
dev.off()
png(filename="data/figures/traffic_ts/diff_traffic_pacf.png")
pacf(diff(traffic_ts), main="Traffic partial autocorrelation after differencing")
dev.off()

png(filename="data/figures/traffic_ts/traffic_bel.png", width=700)
p <- ggplot(traffic_daily, aes(x=day, y=daily_volume)) +
    geom_line() + 
    ggtitle("Daily traffic volume, Belliard Tunnel") +
    xlab("Date") +
    ylab("Daily volume") + theme_bw() +
    theme(title = element_text(size=24), 
          axis.text.y = element_text(size = 22),
          axis.text.x = element_text(size = 22, angle=45, vjust = .6))
p
dev.off()

diff_df <- data.frame(y=diff(traffic_daily$daily_volume))
diff_df$x <- traffic_daily$day[2:nrow(traffic_daily)]
png(filename="data/figures/traffic_ts/diff_traffic_bel.png", width=700)
p <- ggplot(diff_df, aes(x=x, y=y)) +
    geom_line() + 
    ggtitle("Daily traffic volume in differences, Belliard Tunnel") +
    xlab("Date") +
    ylab("Daily volume") +
    theme_bw() +
    theme(title = element_text(size=23), 
          axis.text.y = element_text(size = 22),
          axis.text.x = element_text(size = 22, angle=45, vjust = .6))
p        
dev.off()


# test for non stationarity
cov_max_lag=round(sqrt(length(cov_ts)))
CADFtest(cov_ts, type= "drift", criterion= "BIC", max.lag.y=cov_max_lag)
CADFtest(diff(cov_ts), type= "drift", criterion= "BIC", max.lag.y=cov_max_lag)
# both are stationary

# test for non stationarity
tf_max_lag=round(sqrt(length(traffic_ts)))
CADFtest(traffic_ts, type= "drift", criterion= "BIC", max.lag.y=tf_max_lag)
CADFtest(diff(traffic_ts), type= "drift", criterion= "BIC", max.lag.y=tf_max_lag)
# both are stationary


#############
# VAR model, for each chemical and station of interest
############

# Load selected stations as generated by station_select_for_mvts.R
station_file="data/selected_stations_for_training.txt"
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

adf <- TRUE

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
    predictions_df <- data.frame(observed=vector(mode='numeric',length=length(traffic_ts)))
    adf_tests <- matrix(nrow=nrow(selected_stations), ncol=2)
    for (station_id in 1:nrow(selected_stations)){
        current_station <- air_df %>% dplyr::filter(AirQualityStation %in% selected_stations$station[station_id])
        print(station_id)
        #if (station_id > 1){
        #    break
        #}
        print(selected_stations$station[station_id])
        station_name <- selected_stations$station[station_id]
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
        #station_complete$concentration_inter <- round(na_kalman(station_complete$Concentration, smooth = T),1)
        station_complete$concentration_inter <- round(na_kalman(station_complete$Concentration, smooth = T),4)
        #ggplot_na_imputations(station_complete$Concentration, station_complete$concentration_inter)
        #statsNA(as.matrix(station_complete$concentration_inter))
        # aggregate by day, but need to round up if staying in 2019
        air_daily <- station_complete %>% mutate(day=lubridate::ceiling_date(datetime, "day")) %>% group_by(day) %>% 
          summarise(daily_concentration=mean(concentration_inter)) 
        # create the ts
        air_ts <- ts(air_daily$daily_concentration, frequency=7, start=c(2020, 9))
        # Check for stationarity of srations if specified:
        if (adf == TRUE){
            air_max_lag=round(sqrt(length(air_ts)))
            adf_levels <- CADFtest(air_ts, type= "drift", criterion= "BIC", max.lag.y=air_max_lag)$p.value
            adf_diff <- CADFtest(diff(air_ts), type= "drift", criterion= "BIC", max.lag.y=air_max_lag)$p.value
            adf_tests[station_id, 1] <- adf_levels
            adf_tests[station_id, 2] <- adf_diff
        }

 
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
        
        # Generating predictions
        H <- c(1, 5, 10)
        # Hardcoding based on test split in save_data_for_mvts
        s <- 361
        #s <- round(0.75*length(air_ts))
        dm_tests <- matrix(nrow=length(H), ncol=3)
        oos_errors <- matrix(nrow=3, ncol=5)
        # Add observed values to df for easy error computation in evaluation script
        observed_id <- paste(station_name, "_observed", sep="")
        observed_id
        predictions_df[observed_id] <- air_daily$daily_concentration
        for (i in 1:length(H)){
            h <- H[i]
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
                # The predictions will store all forecats up to/including h
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
            dm_tests[i, 2] <- dm.test(error_cov, error_nocov,h=h,power=1, varestimator = "bartlett")$p.value[[1]]
            dm_tests[i, 3] <- dm.test(error_cov, error_nocov,h=h,power=2, varestimator = "bartlett")$p.value[[1]]
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
    adf_df <- as.data.frame(adf_tests) 
    adf_df$station <- selected_stations$station
    # Move station id to front
    adf_df <- adf_df %>%
        dplyr::select(station, everything()) 
    names(adf_df) <- c("Station", "Levels", "Differences")
    # Nice function for formatting p-values
    adf_df %>% mutate(Levels = scales::pvalue(Levels), Differences = scales::pvalue(Differences)) -> adf_df
    caption <- paste("ADF tests for each station, pollutant ", toupper(chem), sep="")
    label <- paste("adf_chem_", chem, sep="")
    setwd(user_dir)
    adf_file <- paste("data/adf_tables/", label, ".txt", sep="")
    adf_out <- kbl(adf_df, booktabs = T, escape=F, caption=caption, label=label, format="latex") %>% 
              kable_styling(latex_options = "HOLD_position")
    write(adf_out, file=adf_file)
    predictions_df <- predictions_df[!names(predictions_df) %in% c("observed")]
    write.csv(predictions_df, paste("data/model_output/arima/", chem, "_VAR_forecasts.csv", sep=""), row.names = FALSE)
}
        