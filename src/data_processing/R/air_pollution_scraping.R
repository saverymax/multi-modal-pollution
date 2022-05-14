# Script to download air pollution data from EEA
library(tidyverse)
user_dir <- "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution/data/eea_air"
setwd(user_dir)

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
# Even though we scrape for Belgium, the filenames of the data 
# are associated with brussels, a residue of our initial work 
country_names <- c("brux")
urls <- vector(mode="list", length=1)
names(urls) <- country_names
# copenhagen:
bel_url <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?CountryCode=BE&CityName=&Pollutant="
urls[[1]] <- bel_url

for (u in urls){
  for (i in c(pm25, pm10, no2)){
    url <- paste(u, i, "&Year_from=2019&Year_to=2022&Station=&Samplingpoint=&Source=All&Output=HTML&UpdateDate=&TimeCoverage=Year", sep="")
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
    country <- names(urls)[urls==u]
    download_dir <- paste("time_series_", country, "_", i, sep="")
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
