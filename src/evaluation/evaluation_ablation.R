##########################################
# Evaluation script for generating results for variable ablations
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

ablation_analysis <- function(){
    
    var_ablate <- c("all", "covid", "pm10", "pm25")
    learning_rate <- "_lre4" 
    # These were only run with 1 step ahead
    h <- 1
    chem <- "no2"
    oos_errors <- matrix(nrow=41, ncol=4)
    for (v_index in 1:length(var_ablate)){
        v <- var_ablate[v_index]
        if (v == "all"){
            filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", 
                          learning_rate, "_no_station_h-", h, "_", chem, ".csv", sep="")
        } else{
        filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", 
                          learning_rate, "_ablation_", v,"_h-", h, "_", chem, ".csv", sep="")
        }
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
        # And prepare in a matrix for latex output
        oos_errors[, v_index] <- tf_mae_vec
    }
    oos_errors <- round(oos_errors, 3)
    col <- seq_len(ncol(oos_errors))
    for (i in 1:nrow(oos_errors)){
        j <- which.max(oos_errors[i,])
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
    new_names <- c("Station", "All vars", "w/o Covid", "w/o PM10", "w/o PM25")
    colnames(oos_df) <- new_names
    caption <- paste("Out-of-sample errors when variables are iteratively removed, for pollutant ", chem, 
                     ". Results for only horizon 1 are shown.", sep="")
    label <- paste("ablation_", chem, sep="")
    # Need to escape if outputing bold letters.
    print(kbl(oos_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
              kable_styling(latex_options = "HOLD_position")
              )
}
ablation_analysis()

