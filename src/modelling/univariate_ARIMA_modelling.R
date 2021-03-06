# This code is taken from the code I used for the course project from "Advanced Time Series Analysis"
# See https://github.com/saverymax/air-pollution-covid
# It has been adapted deal with a larger number of measuring stations and more closely 
# mirror my work with the transformer model. 
# The code will generate univariate forecasts for each chemical for each measuring station of interest
# However, while it is useful for exploratory modelling, it was not included in the thesis text.

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(CADFtest)
library(vars)
library(imputeTS)
library(kableExtra)
library(forecast)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
# Load selected stations as generated by station_select_for_mvts.R
station_file=paste(user_dir, "/data/selected_stations_for_training.txt")
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

# Though I have the loop, I am just going to look at NO2.
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    if (chem != "NO2"){
        next
    }
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
        if (station_id > 1){
            break
        }
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
        
        # check lags
        acf(air_ts)
        acf(diff(air_ts))
        pacf(air_ts)
        pacf(diff(air_ts))
        acf(diff(air_ts,7))
        pacf(diff(air_ts,7))
        # notice that there is seasonality.
        # take differences and seasonal differences (by day, but has to be in frequency of ts)
        air_2_diff <- diff(diff(air_ts), 7)
        length(air_2_diff)
        air_max_lag <- round(sqrt(length(air_ts)))
        air_max_lag
        
        png(filename=paste(user_dir, "/data/figures/autocorrelations/stations/", chem, "_", station_name, "_", station_id, "_acf_2d", ".png", sep=""))
        acf(air_2_diff)
        dev.off()
        png(filename=paste(user_dir, "/data/figures/autocorrelations/stations/", chem, "_", station_name, "_", station_id, "_pacf_2d", ".png", sep=""))
        pacf(air_2_diff)
        dev.off()
        Box.test(air_ts, lag = air_max_lag, type = "Ljung-Box")
        Box.test(diff(air_ts), lag = air_max_lag, type = "Ljung-Box")
        Box.test(air_2_diff, lag = air_max_lag, type = "Ljung-Box")
        # rejects that is white noise
        
        # test ts for non stationarity
        # don't need type of "trend"
        # reject non-stationary 
        #CADFtest(air_ts, type= "drift", criterion= "BIC", max.lag.y=air_max_lag)
        #CADFtest(diff(air_ts), type= "drift", criterion= "BIC", max.lag.y=air_max_lag)
        #CADFtest(air_2_diff, type= "drift", criterion= "BIC", max.lag.y=air_max_lag)
        
        # this is for the aggregated daily series
        fit1 <- arima(air_ts,order=c(1,1,1), seasonal=c(1,1,1))
        fit2 <- arima(air_ts,order=c(2,1,1), seasonal=c(1,1,1)) 
        fit3 <- arima(air_ts,order=c(3,1,1), seasonal=c(1,1,1)) 
        fit4 <- arima(air_ts,order=c(2,1,1), seasonal=c(0,1,1)) 
        fit5 <- arima(air_ts,order=c(2,1,1), seasonal=c(1,1,0)) 
        fit6 <- arima(air_ts,order=c(3,1,2), seasonal=c(1,1,0)) 
        fit7 <- arima(air_ts,order=c(3,1,1), seasonal=c(2,1,0)) 
        fit8 <- arima(air_ts,order=c(2,1,1), seasonal=c(2,1,1))
        fit9 <- arima(air_ts,order=c(2,1,1), seasonal=c(2,1,2))
        fit10 <- arima(air_ts,order=c(1,0,0), seasonal=c(1,1,1))
        fit11 <- arima(air_ts,order=c(1,0,0), seasonal=c(1,1,0))
        fit12 <- arima(air_ts,order=c(1,0,0), seasonal=c(0,1,1))
        fit13 <- arima(air_ts,order=c(1,0,1), seasonal=c(1,1,1))
        fit14 <- arima(air_ts,order=c(1,0,1), seasonal=c(2,1,1))
        
        white_noise <- c()
        bp <- c()
        for (i in 1:14){
            p_val <- Box.test(eval(as.name(paste("fit", i, sep="")))$residuals, lag = air_max_lag, type = "Ljung-Box")$p.value
            if (p_val >= .05){
                white_noise <- c(white_noise, i)
                bp <- c(bp, p_val)
            }
        }
        model_comparison <- matrix(nrow=length(white_noise), ncol=4)
        model_comparison[,1] <- white_noise
        model_comparison[,2] <- bp
        for (i in 1:length(white_noise)){
            model <- white_noise[i]
            m_call <- eval(as.name(paste("fit", model, sep="")))$call
            print(paste("model", model, ":", "order:", m_call[3], "seasonal:", m_call[4]))
            model_comparison[i, 3] <- AIC(eval(as.name(paste("fit", model, sep=""))))
            model_comparison[i, 4] <- AIC(eval(as.name(paste("fit", model, sep=""))), k=log(length(air_ts)))
            png(filename=paste(user_dir, "/data/figures/autocorrelations/var/", chem, "_model", model, "_", station_name, "_", station_id, "_acf_2d", ".png", sep=""))
            acf(eval(as.name(paste("fit", model, sep="")))$residuals)
            dev.off()
            png(filename=paste(user_dir, "/data/figures/autocorrelations/var/", chem, "_model", model, "_", station_name, "_", station_id, "_acf_2d", ".png", sep=""))
            pacf(eval(as.name(paste("fit", model, sep="")))$residuals)
            dev.off()
        }
        model_comparison_df <- as.data.frame(model_comparison)
        colnames(model_comparison_df) <- c("Model", "Box", "AIC", "SIC")
        model_comparison_df
        print(kbl(model_comparison_df, booktabs = T, format="latex"))
        # this generates all the in-sample statistics
        
        # out of sample computations.
        # compare error with expanding window forecasts
        # Get each model order and season, to fit all of them.
        params <- list()
        for (i in 1:length(white_noise)){
            model_i <- white_noise[i]
            model <- eval(as.name(paste("fit", model_i, sep="")))
            order_call <- model$call[[3]]
            se_call <- model$call[[4]]
            params[[length(params)+1]] <- list(order=c(order_call[[2]], order_call[[3]], order_call[[4]]), 
                                               seasonal=c(se_call[[2]], se_call[[3]], se_call[[4]]))
        }
        
        H <- c(1, 10, 30)
        s <- round(0.75*length(air_ts))
        s
        out_of_sample_error <- matrix(nrow=length(white_noise), ncol=7)
        out_of_sample_error
        # iterate through the models whose residuals were white noise. 
        # for each model, fit for 3 h values
        for (i in 1:length(params)){
            for (j in 1:length(H)){
                if (j==1){
                    error_index <- 2
                }
                else if (j==2){
                    error_index <- 4
                }
                else{
                    error_index <- 6
                }
                error.h<-c()
                h <- H[j]
                # Need to leave h off the end of the time series to be able to predict h ahead.
                for (k in s:(length(air_ts)-h)){
                    # train on expanding window
                    mymodel.sub<-arima(air_ts[1:k], order=params[[i]]$order, seasonal=params[[i]]$seasonal)
                    # predict h step ahead
                    predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
                    # subtract the predicted value predict.h from the observed value, y[i+h]
                    error.h<-c(error.h,air_ts[k+h]-predict.h)
                }
                mae <- mean(abs(error.h))
                # Divide by all y_t
                mape <- mean(abs(error.h)/air_ts[(s+h):length(air_ts)])
                rmse <- sqrt(mean(error.h^2))
                out_of_sample_error[i, 1] <- white_noise[i]
                #out_of_sample_error[i, error_index] <- mae
                out_of_sample_error[i, error_index] <- mape
                out_of_sample_error[i, error_index+1] <- rmse
            }
        }
        
        oos_df <- as.data.frame(out_of_sample_error) 
        colnames(oos_df) <- c("Model", "MAPE, h=1", "RMSE, h=1", 
                              "MAPE, h=10", "RMSE, h=10", 
                              "MAPE, h=30", "RMSE, h=30")
        oos_df
        print(kbl(oos_df, booktabs = T, format="latex"))
        
        # test difference model two well fitting models (12 and 13 in this code)
        # absolute and squared error
        error_1 <-c()
        error_2 <-c()
        h <- 10
        for (k in s:(length(air_ts)-h)){
            fit1_sub <- arima(air_ts[1:k],order=c(1,0,0), seasonal=c(1,1,1))
            fit2_sub <- arima(air_ts[1:k],order=c(1,0,0), seasonal=c(0,1,1))
            # predict h step ahead
            predict_1<-predict(fit1_sub,n.ahead=h)$pred[h]
            predict_2<-predict(fit2_sub,n.ahead=h)$pred[h]
            # subtract the predicted value predict.h from the observed value, y[i+h]
            error_1<-c(error_1,air_ts[k+h]-predict_1)
            error_2<-c(error_2,air_ts[k+h]-predict_2)
        }
        print(dm.test(error_1, error_2,h=h,power=1))
        print(dm.test(error_1, error_2,h=h,power=2))
        
        # Generate plot of forecast
        # using the best model, fit 12.
        # I can get the actual data observed during this period (not included in the analysis above.)
        station_observed <- current_station %>% dplyr::filter(datetime >= "2021-01-02" & datetime <= "2021-02-01")
        # missing some time measurements
        station_obs_complete <- station_observed %>% tidyr::complete(datetime = seq(min(datetime), max(datetime), by="hour"))
        station_obs_complete$concentration_inter <- round(na_kalman(station_obs_complete$Concentration, smooth=T),1)
        statsNA(as.matrix(station_obs_complete$concentration_inter))
        
        # aggregate by day, but need to round up if staying in 2019
        obs_daily <- station_obs_complete %>% mutate(day=lubridate::ceiling_date(datetime, "day")) %>% group_by(day) %>% 
            summarise(daily_concentration=mean(concentration_inter)) 
        obs_daily$daily_concentration
        min(obs_daily$day)
        max(obs_daily$day)
        nrow(obs_daily)
        # create the ts
        obs_ts <- ts(obs_daily$daily_concentration, frequency=7, start=c(2021, 1))
        length(obs_ts)
        ts.plot(obs_ts)
        
        fcast <- predict(fit12,n.ahead=31)
        expected <- fcast$pred
        lower<-fcast$pred-qnorm(0.975)*fcast$se
        upper<-fcast$pred+qnorm(0.975)*fcast$se
        forecast_df <- data.frame(pred=expected, day=obs_daily$day, lower=lower, upper=upper)
        
        png(filename=paste(user_dir, "/data/figures/forecasts/univariate_forecast.png"))
        colors <- c("Observed"="green", "Predicted"="red", "lower"="black", "upper"="black")
        p <- ggplot() +
            geom_line(data=obs_daily, aes(x=day, y=daily_concentration, color="Observed")) + 
            geom_line(data=forecast_df, aes(x=day, y=pred, color="Predicted")) + 
            geom_line(data=forecast_df, aes(x=day, y=lower, color="lower")) + 
            geom_line(data=forecast_df, aes(x=day, y=upper, color="upper")) + 
            ggtitle("Predicted and observed concentration for January-February, 2021") +
            labs(y = "Daily. conc", x = "Day",
                 color = "Legend") +
            scale_color_manual(values = colors)
        p
        dev.off()
        
        
    }
}

