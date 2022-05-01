# Script to download air pollution data from EEA
library(tidyverse)
setwd("D:/linux_documents_11_2021/thesis/code/multi-modal-pollution/data/eea_air")
# Scraping
library(RCurl)
library(httr)
library(XML)
# Queries can be generated at 
# https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm
# The scraping url below is for 2019-2022, E2a, BE, and will provide data from all 
# testing stations. However it's pretty easy to customize
# According to the EEA: 
# Data available in this service comes from two dataflows: 
# E1a and E2a. The E1a data are reported to EEA by memberstates every September 
# and covers the year before the delivery. #
# This means that data delivered in September 2017 covers 2016. 
# EEA also recieves up-to-date (E2a) data on hourly basis from most of its member states.

# Pollutant ids are defined at http://dd.eionet.europa.eu/vocabulary/aq/pollutant/view
# pm2.5 is code 6001
pm25 <- 6001
# pm10 is code 5
pm10 <- 5
# no2 is code 8
no2 <- 8
# nox as no2
nox <- 9
# co2 is code 71 but co2 isn't provided with this service because of differences in the way data is collected
# carbon monoxide
co <- 10
# Sulfur dioxide
so2 <- 1
# This is how we do dictionaries in R.
city_names <- c("copen", "paris", "brux", "ams")
city_urls <- vector(mode="list", length=4)
names(city_urls) <- city_names
# copenhagen:
copen_url <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=DK&CityName=K%C3%B8benhavn&Pollutant="
# paris
paris_url <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=FR&CityName=Paris&Pollutant="
# brussels
brux_url <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=BE&CityName=&Pollutant="
# Amsterdam
ams_url <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=NL&CityName=Amsterdam&Pollutant="
city_urls[[1]] <- copen_url
city_urls[[2]] <- paris_url
city_urls[[3]] <- brux_url
city_urls[[4]] <- ams_url

# Useful if I need to redo just 1
#city_urls <- vector(mode="list", length=1)
#names(city_urls) <- c("paris")
#city_urls[[1]] <- paris_url
#city_urls


for (city_url in city_urls){
  for (i in c(pm25, pm10, no2, co, so2, nox)){
    url <- paste(city_url, i, "&Year_from=2019&Year_to=2022&Station=&Samplingpoint=&Source=All&Output=HTML&UpdateDate=&TimeCoverage=Year", sep="")
    print(url)
    get_out <- POST(url)
    str(content(get_out))
    writeBin(content(get_out, "raw"), "air_quality_query.txt")
    str(content(get_out, "parsed"))
    # Nice reference: https://stackoverflow.com/questions/48257077/read-html-into-r
    html_text <- htmlParse(content(get_out, as='text')) 
    nodes <- getNodeSet(html_text, "//a")
    all_csv <- nodes %>% sapply(xmlValue)
    length(all_csv)
    all_csv[1]
    # Get the name associated with the current URL
    city <- names(city_urls)[city_urls==city_url]
    download_dir <- paste("time_series_", city, "_", i, sep="")
    print(download_dir)
    # Create directory
    dir.create(download_dir, showWarnings = T) 
    cnt=1
    for (i in all_csv){
      download.file(i, file.path(download_dir, paste("airquality_", cnt, ".csv", sep="")), quiet=T)
      Sys.sleep(0.1)
      cnt=cnt+1
    }
  }
}
