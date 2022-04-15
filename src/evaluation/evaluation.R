##########################################
# Evaluation script for all time series forecasts
##########################################

library(tidyverse)
library(ggplot2)
library(stringr)
library(kableExtra)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/src/evaluation"
setwd(user_dir)

chem_names <- c("pm25", "pm10", "no2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

get_arima_preds <- function(df, h){
    # Only need test data
    station_str <- paste("horizon-", h, "_cov", sep="")
    print(station_str)
    df %>% select(ends_with("observed"), c(ends_with(station_str))) -> station_data
    return(station_data)
}

# TODO: Pivot arima into tf_ts format.
arima_df <- get_arima_preds(arima_chem_df, 1)
1:nrow(arima_df)
arima_df$time <- 1:nrow(arima_df)
stopifnot(ncol(arima_df) == 83)
View(arima_df)
pivot_longer(arima_df, cols=names(arima_df))


prepare_transformer <- function(preds){
    # Break up the ids to get just the station name
    split_ts <- str_split(preds$station, "_")
    # Get just the station name and the obs/pred label
    station_ids <- sapply(split_ts, "[", c(1,3))  # Returns matrix
    preds$station_id <- paste(station_ids[1,], station_ids[2,], sep="_")
    # An ok way to do this is to make long first and then go wide
    # This allows the columns to actually correspond to the time series indices (up to 116 
    # but only 94 of those have values in the h=1 case)
    preds_transpose_long <- pivot_longer(preds, cols=colnames(transformer_preds)[colnames(transformer_preds)!=c("station")]) 
    preds_transpose_long %>% group_by(station_id) %>% mutate(index = row_number()) -> preds_transpose_long
    preds_transpose_long %>% pivot_wider(values_from = value, id_cols=station_id, names_from=index) -> preds_transpose_wide
    # Messier way
    #transformer_preds %>% group_by(station_id) %>% mutate(index = row_number()) -> transformer_preds
    #preds_transpose <- pivot_wider(transformer_preds, values_from = X1:X29, id_cols = station_id, names_from = index)
    return(preds_transpose_wide)
}

H <- c(1, 5, 10)
for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    # TODO: Change this
    chem <- names(chem_ids)[chem_index]
    chem_id <-5
    chem="pm10"
    # Arima preds are saved per chem
    arima_chem_df <- read.csv(paste("../../data/model_output/arima/", toupper(chem), "_VAR_forecasts.csv", sep=""), header=T, check.names = F)
    # Only need test data
    arima_chem_df <- arima_chem_df[345:nrow(arima_chem_df),]
    # Have 1 table to compare each chemical, all horizons
    dm_tests <- matrix(nrow=41, ncol=3)
    tf_oos_errors <- matrix(nrow=41, ncol=6)
    ar_oos_errors <- matrix(nrow=41, ncol=6)
    for (h_index in 1:length(H)){
        h <- H[h_index]
        print(chem, h)
        if (chem_id == 5){
            # First handle transformer
            filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval_h-", h, "_", chem, ".csv", sep="")
            transformer_preds <- read.csv(filepath, header=T)
            View(transformer_preds)
            # Combine rows from the same station.
            tf_ts <- prepare_transformer(transformer_preds)
            dim(tf_ts)
            # Split into one df for obs and another for preds
            tf_ts %>% filter(str_detect(station_id, "pred")) %>% arrange(station_id) -> tf_preds 
            tf_ts %>% filter(str_detect(station_id, "obs")) %>% arrange(station_id) -> tf_obs 
            fileout <- paste("../../data/model_output/mvts/transpose_forecast_out_bxl_eval_h-", h, "_", chem, ".csv", sep="")
            write.csv(tf_ts, fileout)
            # Then save as matrix so we can compute error element wise.
            # Dropping station id first, to be added later
            # The following handling is done because of the way I did batches and split up the series. 
            # It is not trivial to handle this in an more elegant way, so I am hardcoding the batch size and
            # splits into this block, unfortunately.
            # This will be 94 instead of 98 - 1 because I split the time series into 4, so we have to take - 4.
            if (h == 1){
                # 29 * 4 + 1 because 29 steps in each series and the station column. 
                # The same logic follows for the rest of the tests
                stopifnot(ncol(tf_preds)==117)
                preds_matrix <- as.matrix(tf_preds[, names(tf_preds) != "station_id"])[, 1:94]
                obs_matrix <- as.matrix(tf_obs[, names(tf_obs) != "station_id"])[, 1:94]
            } else if (h == 5){
                stopifnot(ncol(tf_preds)==101)
                preds_matrix <- as.matrix(tf_preds[, names(tf_preds) != "station_id"])[, 1:81]
                obs_matrix <- as.matrix(tf_obs[, names(tf_obs) != "station_id"])[, 1:81]
            } else{
                stopifnot(ncol(tf_preds)==81)
                preds_matrix <- as.matrix(tf_preds[, names(tf_preds) != "station_id"])[, 1:60]
                obs_matrix <- as.matrix(tf_obs[, names(tf_obs) != "station_id"])[, 1:60]
            }
            # Compute errors
            tf_error <- obs_matrix - preds_matrix
            # Then compute metrics
            mae_vec <- rowMeans(abs(tf_error))
            rmse_vec <- sqrt(rowMeans(tf_error^2))
            if (h_index == 1){
                tf_oos_errors[, h_index] <- rmse_vec
                tf_oos_errors[, h_index+1] <- mae_vec
            } else if (h_index == 2){
                tf_oos_errors[, h_index+1] <- rmse_vec
                tf_oos_errors[, h_index+2] <- mae_vec
            } else {
                tf_oos_errors[, h_index+2] <- rmse_vec
                tf_oos_errors[, h_index+3] <- mae_vec
            }
            
            # Next handle VAR
            #dm_tests[i, 1] <- h
            #dm_tests[i, 2] <- dm.test(error_cov, error_nocov,h=h,power=1)$p.value[[1]]
            #dm_tests[i, 3] <- dm.test(error_cov, error_nocov,h=h,power=2)$p.value[[1]]
        }
    }
    tf_oos_df <- as.data.frame(tf_oos_errors)
    # Station id will be indexed correctly to account for station order
    split_names <- str_split(tf_preds$station_id, "_")
    # Get just the station name and the obs/pred label
    station_names <- sapply(split_names, "[", 1)  # Returns matrix
    tf_oos_df$station <- station_names
    new_names <- c("H-1 RMSE", "H-1 MAE", "H-5 RMSE", "H-5 MAE", "H-10 RMSE", "H-10 MAE", paste("Station measuring", chem))
    colnames(tf_oos_df) <- new_names
    print("Transformer forecasts")
    print(kbl(tf_oos_df, booktabs = T, format="latex"))
}


