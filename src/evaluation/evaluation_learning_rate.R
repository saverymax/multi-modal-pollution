##########################################
# Evaluation script for all time series forecasts
##########################################

library(tidyverse)
library(ggplot2)
library(stringr)
library(kableExtra)
library(CADFtest)

#  Use the development version from github, so we can use the 
# positive-ensured variance estimator
# install.packages("remotes")
# remotes::install_github("robjhyndman/forecast")
library(forecast)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/src/evaluation"
setwd(user_dir)

source("evaluation_data_functions.R")

main_analysis <- function(){
    # To generate tests for stationarity of errors
    run_adf = FALSE
    
    chem_names <- c("pm25", "pm10", "no2")
    chem_ids <- vector(mode="list", length=length(chem_names))
    names(chem_ids) <- chem_names
    chem_ids[[1]] <- 6001
    chem_ids[[2]] <- 5
    chem_ids[[3]] <- 8
     #learning_rate <- "_lre5_no_station" # "" or _lre4 or _lre5 or _lre4_no_station
    learning_rate <- c("_lre4_no_station", "_lre5_no_station")
    for (lr_index in 1:length(learning_rate)){
        lr <- learning_rate[lr_index]
        H <- c(1, 5, 10)
        for (chem_index in 1:length(chem_ids)){
            chem_id <- chem_ids[[chem_index]]
            chem <- names(chem_ids)[chem_index]
            # Arima preds are saved per chem
            arima_chem_df <- read.csv(paste("../../data/model_output/arima/", toupper(chem), "_VAR_forecasts.csv", sep=""), header=T, check.names = F)
            # Only need test data
            # Hardcoded based on test split!
            arima_chem_df <- arima_chem_df[362:nrow(arima_chem_df),]
            # Have 1 table to for each chemical, all horizons
            dm_tests <- matrix(nrow=41, ncol=3)
            stationary_tests <- matrix(nrow=41, ncol=3)
            for (h_index in 1:length(H)){
                # Have an oos matrix for each horizon
                oos_errors <- matrix(nrow=41, ncol=4)
                h <- H[h_index]
                # First handle transformer
                filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", lr, "_h-", h, "_", chem, ".csv", sep="")
                transformer_preds <- read.csv(filepath, header=T)
                # Combine rows from the same station.
                tf_list <- prepare_transformer(transformer_preds)
                tf_preds <- tf_list$tf_preds
                tf_obs <- tf_list$tf_obs
                fileout <- paste("../../data/model_output/mvts/transpose_forecast_out_bxl_eval", lr, "_h-", h, "_", chem, ".csv", sep="")
                write.csv(tf_list$tf_full, fileout)
                
                # Next handle VAR
                arima_df <- get_arima_preds(arima_chem_df, h)
                stopifnot(ncol(arima_df) == 82)
                arima_transpose <- t(arima_df)
                arima_t_df <- as.data.frame(arima_transpose)
                arima_t_df$station_id <- rownames(arima_transpose)
                rownames(arima_t_df) <- NULL
                # Station id will be the last column so set seq id up to that.
                names(arima_t_df)[1:nrow(arima_df)] <- 1:nrow(arima_df)
                # Move station id to front
                arima_t_df <- arima_t_df %>%
                    select(station_id, everything())
                # Now, I need to remove the observations from arima that will not correspond to those from tf
                # This is an artifact of splitting the tf predictions into subsets of 30 seq.
                arima_t_df %>% filter(str_detect(station_id, "horizon")) %>% arrange(station_id) -> arima_preds 
                arima_t_df %>% filter(str_detect(station_id, "observed")) %>% arrange(station_id) -> arima_obs 
                arima_preds_matrix <- as.matrix(arima_preds[, names(arima_preds) != "station_id"])
                arima_obs_matrix <- as.matrix(arima_obs[, names(arima_obs) != "station_id"])
                # Now drop the values that won't correspond to those in the tf forecast. It's easier to do this 
                # as a matrix.
                arima_preds_mx_clean <- drop_arima_elements(arima_preds_matrix, h)
                arima_obs_mx_clean <- drop_arima_elements(arima_obs_matrix, h)
                # Write transpose out
                fileout_var <- paste("../../data/model_output/arima/transpose_forecast_out_h-", h, "_", chem, ".csv", sep="")
                write.csv(arima_t_df, fileout_var)
                
                # Then save var and tf matrix so we can compute error element wise.
                # Dropping station id first, to be added later
                tf_matrices <- check_tf_matrices(h, tf_preds, tf_obs) 
                tf_preds_matrix <- tf_matrices$tf_preds_matrix
                tf_obs_matrix <- tf_matrices$tf_obs_matrix
                
                # Dimensions should be (41, 94) for h=1, (41, 78) for h=5, and (41, 60) for h=10
                stopifnot(dim(arima_preds_mx_clean) == dim(tf_preds_matrix))
                stopifnot(dim(arima_obs_mx_clean) == dim(tf_obs_matrix))
                # This will round to allow for the floating point differences but still providing some accountability.
                stopifnot(round(tf_obs_matrix) == round(arima_obs_mx_clean))
                #which(round(tf_obs_matrix, 1) != round(arima_obs_mx_clean, 1))
                
                # Compute errors
                tf_error <- tf_obs_matrix - tf_preds_matrix
                var_error <- arima_obs_mx_clean - arima_preds_mx_clean
                # Then compute metrics
                tf_mae_vec <- rowMeans(abs(tf_error))
                tf_rmse_vec <- sqrt(rowMeans(tf_error^2))
                var_mae_vec <- rowMeans(abs(var_error))
                var_rmse_vec <- sqrt(rowMeans(var_error^2))
                # And prepare in a matrix for latex output
                oos_errors[, 1] <- tf_rmse_vec
                oos_errors[, 2] <- tf_mae_vec
                oos_errors[, 3] <- var_rmse_vec
                oos_errors[, 4] <- var_mae_vec
                oos_errors <- round(oos_errors, 3)
                # Bold the lowest RMSE and MAE
                for (i in 1:nrow(oos_errors)){
                    j1 <- which.min(c(oos_errors[i,1], oos_errors[i,3]))
                    j2 <- which.min(c(oos_errors[i,2], oos_errors[i,4]))
                    if (j1 == 1){
                        oos_errors[i,1] <- oos_errors[i,1] %>% cell_spec(bold = T)
                    } else {
                        
                        oos_errors[i,3] <- oos_errors[i,3] %>% cell_spec(bold = T)
                    }
                    if (j2 == 1){
                        oos_errors[i,2] <- oos_errors[i,2] %>% cell_spec(bold = T)
                    } else {
                        oos_errors[i,4] <- oos_errors[i,4] %>% cell_spec(bold = T)
                    }
                }
                oos_df <- as.data.frame(oos_errors)
                # Station id will be indexed correctly to account for station order
                split_names <- str_split(tf_preds$station_id, "_")
                # Get just the station name and the obs/pred label
                station_names <- sapply(split_names, "[", 1)  # Returns matrix
                oos_df$station <- station_names
                # Move station id to front
                oos_df <- oos_df %>%
                    select(station, everything())
                new_names <- c("Station", "TF RMSE", "TF MAE", "VAR RMSE", "VAR MAE")
                #new_names <- c("TF H-1 RMSE", "TF H-1 MAE", "VAR H-1 RMSE", "VAR H-1 MAE", 
                #               "TF H-5 RMSE", "TF H-5 MAE", "VAR H-5 RMSE", "VAR H-5 MAE", 
                #               "TF H-10 RMSE", "TF H-10 MAE", "VAR H-10 RMSE", "VAR H-10 MAE", 
                #               paste("Station measuring", chem))
                #new_names <- c("H-1 RMSE", "H-1 MAE", "H-5 RMSE", "H-5 MAE", "H-10 RMSE", "H-10 MAE", paste("Station measuring", chem))
                colnames(oos_df) <- new_names
                lr_check <- grepl("5", lr)
                if (lr_check == T){
                    lr_text <- "1e-5"
                } else{
                   lr_text <- "1e-4" 
                }
                caption <- paste("Out-of-sample errors for ", toupper(chem), ", horizon ", h, 
                             "and learning rate ", lr_text, " compared to VAR model.", sep="")
                label <- paste("lr-", lr_text, "_", chem, "_h-", h, sep="")
                print(kbl(oos_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
                  kable_styling(latex_options = "HOLD_position")
                  )
                
                # Finally, compute diebold mariano with bartlett variance estimator for each station, given a horizon.
                dm_vec <- mapply(function(tf, var){dm.test(tf, var, h=h, power=1, varestimator="bartlett")$p.value[[1]]}, 
                                split(tf_error, row(tf_error)), split(var_error, row(var_error)))
                dm_tests[, h_index] <- dm_vec
                # Option because rather slow to run
                if (run_adf==TRUE){
                    stationary_test_vec <- mapply(function(tf, var){
                        CADFtest(tf-var, type= "drift", criterion= "BIC", max.lag.y=round(sqrt(length(var))))$p.value}, 
                        split(tf_error, row(tf_error)), split(var_error, row(var_error)))
                    stationary_tests[, h_index] <- stationary_test_vec
                }
                # When not using bartlett variance, this block can check which series lead to negative variance
                #options(warn=2)
                #for (i in 1:41){
                #    dm_row <- dm.test(tf_error[i,], var_error[i,], h=h, power=1, varestimator="bartlett")$p.value[[1]]
                #    dm_tests[i, h_index] <- dm_row
                #        }
            }
        
            # Then prepare the DM output 
            dm_tests <- round(dm_tests, 3)
            col <- seq_len(ncol(dm_tests))
            for (i in 1:nrow(dm_tests)){
                sig_i <- which(dm_tests[i,] < .05)
                # This will return the indices for which the p value is less than .05
                # Then iterate through and set the cells.
                for (k_index in 1:length(sig_i)){
                    k <- sig_i[k_index]
                    dm_tests[i,k] <- dm_tests[i,k] %>% cell_spec(bold = T)
                }
            }
            dm_df <- as.data.frame(dm_tests)
            dm_df$station <- station_names
            # Move station id to front
            dm_df <- dm_df %>%
                select(station, everything())
            names(dm_df) <- c("Station", "H1", "H5", "H10")
            caption <- "Diebold Mariano tests for comparing VAR and transformer between stations and horizons"
            label <- paste("dm_tests_lr-", lr_text, "_", chem, sep="")
                print(kbl(oos_df, booktabs = t, escape=f, caption=caption, label=label) %>% 
                  kable_styling(latex_options = "hold_position")
                  )
                
            if (run_adf==t){
                stationary_df <- as.data.frame(stationary_tests)
                stationary_df$station <- station_names
                stationary_df <- stationary_df %>%
                    select(station, everything())
                names(stationary_df) <- c("station", "h1", "h5", "h10")
                print("augmented dickey fuller test for stationarity")
                print(kbl(stationary_df, booktabs = t))
            }
        }
    }
}
main_analysis()

