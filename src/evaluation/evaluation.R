##########################################
# Evaluation script for all time series forecasts
##########################################

library(tidyverse)
library(ggplot2)
library(stringr)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/src/evaluation"
setwd(user_dir)

chem_names <- c("pm25", "pm10", "no2")
chem_ids <- vector(mode="list", length=length(chem_names))
names(chem_ids) <- chem_names
chem_ids[[1]] <- 6001
chem_ids[[2]] <- 5
chem_ids[[3]] <- 8

prepare_transformer <- function(preds){
    # Break up the ids to get just the station name
    split_ts <- str_split(preds$station, "_")
    # Get just the station name and the obs/pred label
    station_ids <- sapply(split_ts, "[", c(1,3))  # Returns matrix
    preds$station_id <- paste(station_ids[1,], station_ids[2,], sep="_")
    # An ok way to do this is to make long first and then go wide
    # This allows the columns to actually correspond to the time series indices (up to 116)
    preds_transpose_long <- pivot_longer(preds, cols=X1:X29)
    preds_transpose_long %>% group_by(station_id) %>% mutate(index = row_number()) -> preds_transpose_long
    preds_transpose_long %>% pivot_wider(values_from = value, id_cols=station_id, names_from=index) -> preds_transpose_wide
    # Messier way
    #transformer_preds %>% group_by(station_id) %>% mutate(index = row_number()) -> transformer_preds
    #preds_transpose <- pivot_wider(transformer_preds, values_from = X1:X29, id_cols = station_id, names_from = index)
    return(preds_transpose_wide)
}

for (chem_index in 1:length(chem_ids)){
    chem_id <- chem_ids[[chem_index]]
    chem <- names(chem_ids)[chem_index]
    H <- c(1, 5, 10)
    for (h in 1:length(H)){
        if (h==1 & chem_id == 5){
            filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval_h-", h, "_", chem, ".csv", sep="")
            transformer_preds <- read.csv(filepath, header=T)
            head(transformer_preds)
            # Combine rows from the same station.
            tf_ts <- prepare_transformer(transformer_preds)
            tf_ts <- tf_ts[,1:94]
            # Splite into one df for obs and another for preds
            tf_ts %>% filter(str_detect(station_id, "pred")) %>% arrange(station_id) -> tf_preds 
            tf_ts %>% filter(str_detect(station_id, "obs")) %>% arrange(station_id) -> tf_obs 
            fileout <- paste("../../data/model_output/mvts/transpose_forecast_out_bxl_eval_h-", h, "_", chem, ".csv", sep="")
            write.csv(tf_ts, fileout)
            # Then save as matrix so we can compute error element wise.
            # Dropping station id first, to be added later
            preds_matrix <- as.matrix(tf_preds[, names(tf_preds) != "station_id"])[, 1:94]
            obs_matrix <- as.matrix(tf_obs[, names(tf_obs) != "station_id"])[, 1:94]
            # Compute errors
            tf_error <- obs_matrix - preds_matrix
            #masked_indices <- which(obs_matrix==0,arr.ind = T)
            #tf_error[masked_indices] = NA
            mae_mat <- rowMeans(abs(tf_error))
            mape_mat <- rowMeans(abs(tf_error)/obs_matrix)
            # TODO: We have a little problem of a 0 observed value for a day, not sure how that happened.
            # Well, the issue is that sometimes there are juts measurements of 0 in the raw data. Took me 
            # a while to sort through that. So we will just have to accept it and move on.
            #obs_matrix[which(obs_matrix==0, arr.ind=T)]
            mape_df <- as.data.frame(mape_mat)
            mape_df$station <- tf_preds$station_id
            mape_df
            rmse_mat <- sqrt(rowMeans(tf_error^2))
            rmse_mat
            mae_mat
            # TODO: Get these metrics for each horizon. Then add them each to a dataframe, for each station, 
            # Then, get the same thing for arima. Then compute dm.
            #dm_tests[i, 1] <- h
            #dm_tests[i, 2] <- dm.test(error_cov, error_nocov,h=h,power=1)$p.value[[1]]
            #dm_tests[i, 3] <- dm.test(error_cov, error_nocov,h=h,power=2)$p.value[[1]]
            # get arima forecast for that station
            #arima_preds <- read.csv("../../data/model_output/arima")
        }
    }
}


