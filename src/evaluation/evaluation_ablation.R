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
    
    var_ablate <- c("covid", "pm10", "pm25", "tunnels", "all")
    learning_rate <- "_lre4" 
    # These were only run with 1 step ahead
    h <- 1
    chem <- "no2"
    dm_tests <- matrix(nrow=41, ncol=5)
    oos_errors <- matrix(nrow=41, ncol=6)
    # Get errors from original run to compare with original
    filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", 
                  learning_rate, "_no_station_h-", h, "_", chem, ".csv", sep="")
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
    # Compute errors
    original_error <- tf_obs_matrix - tf_preds_matrix
    tf_mae_vec <- rowMeans(abs(original_error))
    # And put in the first column for latex output
    oos_errors[, 1] <- tf_mae_vec
    for (v_index in 1:length(var_ablate)){
        v <- var_ablate[v_index]
        filepath <- paste("../../data/model_output/mvts/forecast_out_bxl_eval", 
                          learning_rate, "_ablation_", v,"_h-", h, "_", chem, ".csv", sep="")
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
        oos_errors[, v_index+1] <- tf_mae_vec
        
        # Dm test between ablated and original:
        dm_vec <- mapply(function(tf_1, tf_2){dm.test(tf_1, tf_2, h=h, power=1, varestimator="bartlett")$p.value[[1]]}, 
                         split(tf_error, row(tf_error)), split(original_error, row(original_error)))
        dm_tests[, v_index] <- dm_vec
        
    }
    oos_errors <- round(oos_errors, 3)
    col <- seq_len(ncol(oos_errors))
    col
    # Bold the largest increases
    for (i in 1:nrow(oos_errors)){
        j <- which.max(oos_errors[i,])
        oos_errors[i,] <- oos_errors[i,] %>% cell_spec(bold = col == j)
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
    new_names <- c("Station", "None removed", "COVID", "PM10", "PM25", "Tunnels", "Only NO2") 
    colnames(oos_df) <- new_names
    caption <- paste("Out-of-sample MAE when variables are removed, 
                     when forecasting pollutant ", toupper(chem), 
                     ". Results for only horizon 1 are shown. 
                     Values with highest MAE per station are in bold. Name of variable indicates its removal from input
                     except in the case of Only NO2, which indicates univariate forecasting of NO2.", sep="")
    label <- paste("ablation_", chem, sep="")
    # Need to escape if outputing bold letters.
    print(kbl(oos_df, booktabs = T, escape=F, caption=caption, label=label, 
              align=c("lcccccc"), digits=4) %>% 
              kable_styling(latex_options = "HOLD_position")
              )
    # Then prepare the DM output 
    dm_tests <- round(dm_tests, 3)
    dm_df <- as.data.frame(dm_tests)
    dm_df$station <- station_names
    # Move station id to front
    dm_df <- dm_df %>%
        select(station, everything())
    names(dm_df) <- c("Station", "COVID", "PM10", "PM25", "Tunnels", "OnlyNO2")
    dm_df %>% mutate(COVID=scales::pvalue(COVID), PM10=scales::pvalue(PM10), PM25=scales::pvalue(PM25),
        Tunnels=scales::pvalue(Tunnels), OnlyNO2=scales::pvalue(OnlyNO2)) -> dm_df
    for (i in 1:nrow(dm_df)){
        sig_i <- which(dm_df[i,] < .05)
        # This will return the indices for which the p value is less than .05
        # Then iterate through and set the cells.
        if (length(sig_i) != 0){
            for (k_index in 1:length(sig_i)){
                k <- sig_i[k_index]
                dm_df[i,k] <- dm_df[i,k] %>% cell_spec(bold = T)
            }
        }
    }
    caption <- paste("Diebold Mariano tests comparing model with removed variable to model with all variables, 
                         for pollutant ", toupper(chem), sep="")
    label <- paste("dm_ablation_", chem, sep="")
    print(kbl(dm_df, booktabs = T, escape=F, caption=caption, label=label) %>% 
              kable_styling(latex_options = "HOLD_position")
    )
}
ablation_analysis()

