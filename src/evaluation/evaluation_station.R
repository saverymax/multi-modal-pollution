##########################################
# Evaluation script for generating results for station comparison
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

station_analysis <- function(){
    # To generate tests for stationarity of errors
    run_adf = FALSE
    
    chem_names <- c("pm25", "pm10", "no2")
    chem_ids <- vector(mode="list", length=length(chem_names))
    names(chem_ids) <- chem_names
    chem_ids[[1]] <- 6001
    chem_ids[[2]] <- 5
    chem_ids[[3]] <- 8
    learning_rate <- "_lre4" 
    
    H <- c(1, 5, 10)
    for (chem_index in 1:length(chem_ids)){
        chem_id <- chem_ids[[chem_index]]
        chem <- names(chem_ids)[chem_index]
        # Only need test data
        # Have 1 table to for each chemical, all horizons
        dm_tests <- matrix(nrow=41, ncol=3)
        stationary_tests <- matrix(nrow=41, ncol=3)
        for (h_index in 1:length(H)){
            # Have an oos matrix for each horizon
            oos_errors <- matrix(nrow=41, ncol=4)
            h <- H[h_index]
            # First handle transformer
            filepath_station <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", learning_rate, "_station_h-", h, "_", chem, ".csv", sep="")
            filepath_no_station <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", learning_rate, "_no_station_h-", h, "_", chem, ".csv", sep="")
            transformer_preds_station <- read.csv(filepath_station, header=T)
            transformer_preds_no_station <- read.csv(filepath_no_station, header=T)
            # Combine rows from the same station.
            tf_list_s <- prepare_transformer(transformer_preds_station)
            tf_preds_s <- tf_list_s$tf_preds
            tf_obs_s <- tf_list_s$tf_obs
            tf_list_ns <- prepare_transformer(transformer_preds_no_station)
            tf_preds_ns <- tf_list_ns$tf_preds
            tf_obs_ns <- tf_list_ns$tf_obs
            
            # Then save station and no station matrix so we can compute error element wise.
            # See evaluation_learning_rate for description
            tf_matrices_s <- check_tf_matrices(h, tf_preds_s, tf_obs_s) 
            tf_preds_matrix_s <- tf_matrices_s$tf_preds_matrix
            tf_obs_matrix_s <- tf_matrices_s$tf_obs_matrix
            tf_matrices_ns <- check_tf_matrices(h, tf_preds_ns, tf_obs_ns) 
            tf_preds_matrix_ns <- tf_matrices_ns$tf_preds_matrix
            tf_obs_matrix_ns <- tf_matrices_ns$tf_obs_matrix
                
            # Dimensions should be (41, 94) for h=1, (41, 78) for h=5, and (41, 60) for h=10
            stopifnot(dim(tf_preds_matrix_s) == dim(tf_preds_matrix_ns))
            stopifnot(dim(tf_obs_matrix_s) == dim(tf_obs_matrix_ns))
            
            # Compute errors
            tf_error_s <- tf_obs_matrix_s - tf_preds_matrix_s
            tf_error_ns <- tf_obs_matrix_ns - tf_preds_matrix_ns
            # Then compute metrics
            tf_mae_vec_s <- rowMeans(abs(tf_error_s))
            tf_rmse_vec_s <- sqrt(rowMeans(tf_error_s^2))
            tf_mae_vec_ns <- rowMeans(abs(tf_error_ns))
            tf_rmse_vec_ns <- sqrt(rowMeans(tf_error_ns^2))
            # And prepare in a matrix for latex output
            oos_errors[, 1] <- tf_rmse_vec_s
            oos_errors[, 2] <- tf_mae_vec_s
            oos_errors[, 3] <- tf_rmse_vec_ns
            oos_errors[, 4] <- tf_mae_vec_ns
            
            oos_errors <- round(oos_errors, 3)
                #apply(oos_errors, which.min, margin=1) 
            for (i in 1:nrow(oos_errors)){
                j1 <- which.min(c(oos_errors[i,1], oos_errors[i,3]))
                j2 <- which.min(c(oos_errors[i,2], oos_errors[i,4]))
                if (j1 == 1){
                    oos_errors[i,1] <- oos_errors[i,1] %>% cell_spec(bold = 1)
                } else {
                    oos_errors[i,3] <- oos_errors[i,3] %>% cell_spec(bold = 3)
                }
                if (j2 == 1){
                    oos_errors[i,2] <- oos_errors[i,2] %>% cell_spec(bold = 2)
                } else {
                    oos_errors[i,4] <- oos_errors[i,4] %>% cell_spec(bold = 4)
                }
            }
            oos_df <- as.data.frame(oos_errors)
            # Station id will be indexed correctly to account for station order
            split_names <- str_split(tf_preds_s$station_id, "_")
            # Get just the station name and the obs/pred label
            station_names <- sapply(split_names, "[", 1)  # Returns matrix
            oos_df$station <- station_names
            # Move station id to front
            oos_df <- oos_df %>%
                select(station, everything())
            new_names <- c("Station", "TF w/ station RMSE", "TF w/ station MAE", "Tf w/o station RMSE", "Tf w/o station MAE")
            #new_names <- c("TF H-1 RMSE", "TF H-1 MAE", "VAR H-1 RMSE", "VAR H-1 MAE", 
            #               "TF H-5 RMSE", "TF H-5 MAE", "VAR H-5 RMSE", "VAR H-5 MAE", 
            #               "TF H-10 RMSE", "TF H-10 MAE", "VAR H-10 RMSE", "VAR H-10 MAE", 
            #               paste("Station measuring", chem))
            #new_names <- c("H-1 RMSE", "H-1 MAE", "H-5 RMSE", "H-5 MAE", "H-10 RMSE", "H-10 MAE", paste("Station measuring", chem))
            colnames(oos_df) <- new_names
            caption <- paste("Out-of-sample errors for", chem, "for horizon", h, 
                             "when comparing effect of Station identification variable.")
            label <- paste("station_", chem, "_h-", h, sep="")
            print(kbl(oos_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
              kable_styling(latex_options = "HOLD_position")
              )
            
            # Finally, compute diebold mariano with bartlett variance estimator for each station, given a horizon.
            dm_vec <- mapply(function(tf_s, tf_ns){dm.test(tf_s, tf_ns, h=h, power=1, varestimator="bartlett")$p.value[[1]]}, 
                            split(tf_error_s, row(tf_error_s)), split(tf_error_ns, row(tf_error_ns)))
            dm_tests[, h_index] <- dm_vec
            # Option because rather slow to run
            if (run_adf==TRUE){
                stationary_test_vec <- mapply(function(tf_s, tf_ns){
                    CADFtest(tf_s-tf_ns, type= "drift", criterion= "BIC", max.lag.y=round(sqrt(length(var))))$p.value}, 
                    split(tf_error_s, row(tf_error_s)), split(tf_error_ns, row(tf_error_ns)))
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
        print(paste("Tests for chem", chem))
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
        caption <- paste("Diebold Mariano tests comparing models with and without station variable for each horizon")
        label <- paste("dm_station_", chem, sep="")
        print(kbl(dm_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
          kable_styling(latex_options = "HOLD_position")
          )
        if (run_adf==T){
            stationary_df <- as.data.frame(stationary_tests)
            stationary_df$station <- station_names
            stationary_df <- stationary_df %>%
                select(station, everything())
            names(stationary_df) <- c("Station", "H1", "H5", "H10")
            caption = "Augmented Dickey Fuller test for stationarity"
            label <- paste("adf_station_", chem, sep="")
            print(kbl(stationary_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
              kable_styling(latex_options = "HOLD_position")
              )
        }
    }
}
station_analysis()

