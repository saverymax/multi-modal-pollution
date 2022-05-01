############################################################################
# Script to generate time series of traffic monitoring stations in Brussels.
############################################################################

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(imputeTS)

user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
setwd(user_dir)
tunnels <- c("Tun VP - A12", "Tun Del - Parking",  "Tun Montg - Cambre", 
             "Tun Ste OUT - Centre et Bas - Cambre", "Tun Lou IN - Bas - Midi et Cambre") 

traffic_df <- read_excel(
        path="data/traffic/20210921-Tunnels2019-sept2021.xlsx"
        ) 
dim(traffic_df)
names(traffic_df) <- tolower(names(traffic_df))
# first need to select the detector to use.
traffic_summary <- traffic_df %>% dplyr::group_by(detector) %>% 
  summarise(avg_tf=mean(volume, na.rm=T), sd_tf=sd(volume, na.rm=T), len=n()) %>% 
  arrange(desc(avg_tf))
traffic_summary$detector
# using tun bel in because it has data for longest period with a high average
#detector_df <- traffic_df %>% dplyr::filter(detector=="Tun Bel IN")
#detector_df <- traffic_df %>% dplyr::filter(detector %in% tunnels) 
detector_df <- traffic_df
dim(detector_df)
head(detector_df)
detector_df$from <- ymd_hms(detector_df$from)
min(detector_df$from)
max(detector_df$from)
# this detector starts at 2019-01-01 00:00:00
# day duration counter https://www.timeanddate.com/date/duration.html?d1=01&m1=01&y1=2019&d2=&m2=&y2=&ti=on&
detector_subset <- detector_df %>% dplyr::filter(from > "2020-03-01 00:00:00" & from <= "2021-06-01")
min(detector_subset$from)
max(detector_subset$from)
nrow(detector_subset)

# fill in missing dates
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
detector_complete <- detector_subset %>% group_by(detector) %>% tidyr::complete(from = seq(min(from), max(from), by="hour"))
detector_complete
min(detector_complete$from)
max(detector_complete$from)
nrow(detector_complete)

# run imputations
detector_complete <- detector_complete %>% 
    group_by(detector) %>% 
    mutate(volume_inter=round(na_interpolation(volume), 4))

detector_complete
statsNA(as.matrix(detector_complete$volume_inter)) 

# aggregate by day, but need to round up if staying in 2019
traffic_daily <- detector_complete %>% group_by(detector) %>% mutate(day=lubridate::ceiling_date(from, "day")) %>% group_by(day, .add=T) %>% 
  summarise(daily_volume=mean(volume_inter)) 

traffic_daily %>% group_by(detector) %>% summarise(total_avg=mean(daily_volume))
traffic_daily %>% group_by(detector) %>% summarise(n = n(), min=min(day), max=max(day))

tunnel_filenames <- gsub("__", "_", gsub(" ", "_", gsub("-", "", tolower(tunnels))))

lockdown_regions <- data.frame(xmin=c(as.POSIXlt("2020-03-18"), as.POSIXlt("2020-10-28")), 
                               xmax=c(as.POSIXlt("2020-05-06"), as.POSIXlt("2021-05-08")), 
                               ymin=-Inf, ymax=Inf)
for (t_index in 1:length(tunnels)){
    tunnel <- tunnels[t_index]
    tunnel_file <- tunnel_filenames[t_index]
    png(filename=paste(user_dir, "/data/figures/traffic_ts/", tunnel_file, "_ts.png", sep=""), width=700)
    traffic_daily %>% dplyr::filter(detector==tunnel) %>% 
        ggplot(aes(x=day, y=daily_volume)) +
        geom_line() + 
        ggtitle(paste(tunnel)) +
        xlab("Date") +
        ylab("Avg. volume") + theme_bw(base_size=22) + 
        theme(title = element_text(size=20), 
              axis.text.y = element_text(size = 20), 
              axis.text.x = element_text(size = 20, angle=45, vjust = .6)) +
        geom_rect(data=lockdown_regions, alpha=.3, inherit.aes=FALSE, 
                  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="steelblue3") -> p
    print(p)
    dev.off()
}
tunnels <- unique(traffic_daily$detector)
tunnel_filenames <- gsub("\\.", "", gsub("__", "_", gsub(" ", "_", gsub("-", "", tolower(tunnels)))))
for (t_index in 1:length(tunnels)){
    tunnel <- tunnels[t_index]
    tunnel_file <- tunnel_filenames[t_index]
    png(filename=paste(user_dir, "/data/figures/traffic_ts/", tunnel_file, "_ts.png", sep=""), width=700)
    traffic_daily %>% dplyr::filter(detector==tunnel) %>% 
        ggplot(aes(x=day, y=daily_volume)) +
        geom_line() + 
        ggtitle(paste(tunnel)) +
        xlab("Date") +
        ylab("Avg. volume") + theme_bw(base_size=22) + 
        theme(title = element_text(size=20), 
              axis.text.y = element_text(size = 20), 
              axis.text.x = element_text(size = 20, angle=45, vjust = .6)) +
        geom_rect(data=lockdown_regions, alpha=.3, inherit.aes=FALSE, 
                  aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="steelblue3") -> p
    print(p)
    dev.off()
}



