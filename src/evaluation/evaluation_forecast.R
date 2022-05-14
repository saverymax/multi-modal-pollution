###########################################################
# Evaluation script for generating results for removing
# the forecast mask
###########################################################

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

forecast_analysis <- function(){
    
    learning_rate <- "_lre4" 
    chem_names <- c("pm25", "pm10", "no2")
    # These experiments were run with 0 and 1 horizons
    H <- c(0, 1)
    masks <- c("mask", "mask_none")
    oos_col_names <- vector(length=5)
    oos_col_names[1] <- "Station"
    # One table for each chemical, containing mask/no mask, h=0/h=1
    for (c_index in 1:length(chem_names)){
        oos_errors <- matrix(nrow=41, ncol=4)
        chem <- chem_names[c_index]
        #print(paste("Forecasting mask effect on", chem))
        e_index <- 0
        for (h_index in 1:length(H)){
            h <- H[h_index]
            for (m_index in 1:length(masks)){
                mask <- masks[m_index]
                e_index <- e_index + 1
                filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", 
                                  learning_rate, "_forecast_", mask, "_h-", h, "_", chem, ".csv", sep="")
                transformer_preds <- read.csv(filepath, header=T)
                # Combine rows from the same station.
                tf_list <- prepare_transformer(transformer_preds)
                tf_preds <- tf_list$tf_preds
                tf_obs <- tf_list$tf_obs
                
                # Then save station and no station matrix so we can compute error element wise.
                # See evaluation_learning_rate for description
                tf_matrices <- check_tf_matrices(h, tf_preds, tf_obs) 
                tf_preds_matrix <- tf_matrices$tf_preds_matrix
                tf_obs_matrix <- tf_matrices$tf_obs_matrix
                # Dimensions should be (41, 94) for h=1, (41, 78) for h=5, and (41, 60) for h=10
                
                # Compute errors
                tf_error <- tf_obs_matrix - tf_preds_matrix
                tf_mae_vec <- rowMeans(abs(tf_error))
                # latex output breaks when there is _
                oos_errors[, e_index] <- tf_mae_vec
                # Format for column names
                if (mask == "mask_none"){
                    mask = "No mask"
                } else{
                    mask = "Mask"
                }
                oos_col_names[e_index+1] <- paste(mask, sep=" ")
                #oos_col_names[e_index+1] <- paste("Horizon", h, mask, sep=" ")
            }
        }
        #apply(oos_errors, which.min, margin=1) 
        oos_errors <- round(oos_errors, 3)
        col <- seq_len(ncol(oos_errors))
        for (i in 1:nrow(oos_errors)){
            j <- which.min(oos_errors[i,])
            oos_errors[i,] <- oos_errors[i,] %>% cell_spec(bold = col == j)
        }
        #oos_errors %>% kable(booktabs = TRUE, escape = FALSE) 
        oos_df <- as.data.frame(oos_errors)
        # Station id will be indexed correctly to account for station order
        split_names <- str_split(tf_preds$station_id, "_")
        # Get just the station name and the obs/pred label
        station_names <- sapply(split_names, "[", 1)  # Returns matrix
        oos_df$station <- station_names
        # Move station id to front
        oos_df <- oos_df %>%
            select(station, everything())
        colnames(oos_df) <- oos_col_names
        caption <- paste("Out-of-sample MAE when forecast mask is removed, for pollutant ", 
                         toupper(chem),". Lowest MAE is in bold, per station", sep="")
        label <- paste("forecast_mask_", chem, sep="")
        print(kbl(oos_df, booktabs = T, escape=F, caption=caption, label=label, align=c("lcccc"), digits=4) %>% 
                  kable_styling(latex_options = "HOLD_position") %>% 
                  add_header_above(c(" " = 1, "H=0" = 2, "H=1" = 2))
                  )
    }        
}
forecast_analysis()

