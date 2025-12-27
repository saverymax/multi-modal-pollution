# Data
This section describes the data and the data collection process for collecting the data necessary to forecast air pollution in Belium.

All data except the EEA air pollution data can be found in the [data repository](https://github.com/saverymax/multi-modal-pollution/tree/main/data). The EEA data for Belgium can be found at [this OSF repository](https://osf.io/j94pv/). Use of the data files will be provided in the modelling and experimentation sections of this documentation.

## EEA Air pollution

The data can be downloaded [here](https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm)
9-18 using the air quality measurement stations as mandated by Directive
2008/50/EC of the European Parliament. [This script](https://github.com/saverymax/multi-modal-pollution/blob/main/src/data_processing/R/air_pollution_scraping.R) will scrape the data automatically for Belgium. Note that the data in the OSF repository is the product of this script and can be fed directly to the [VAR model](https://github.com/saverymax/multi-modal-pollution/blob/main/src/modelling/VAR_modelling.R) or to the data processing script described below.

The air pollution data can visualized with [this script](https://github.com/saverymax/multi-modal-pollution/blob/main/src/data_processing/R/belgium_pollutants_ts_plots.R).

## COVID-19

Belgium COVID-19 data is available from [Sciensano](https://epistat.sciensano.be/Data/COVID19BE.xlsx). The description of the variables can be found [in this code book](https://epistat.sciensano.be/COVID19BE_codebook.pdf) and other [dataset information here](https://epistat.wiv-isp.be/covid/).

We provide the data in [this repository](https://github.com/saverymax/multi-modal-pollution/tree/main/data/covid).

## Traffic volume
Data provided by Bruxelles Mobilite upon request, [available here](https://github.com/saverymax/multi-modal-pollution/tree/main/data/traffic).

We use five tunnels from Brussels, as described in the thesis and visualized in [this script](https://github.com/saverymax/multi-modal-pollution/blob/main/src/data_processing/R/brussels_traffic_station_plots.R). The tunnels of interest can be [visualized here](https://data.mobility.brussels/mobigis/?x=485351&y=6593040&zoom=12&baselayer=urbis_grey&layers=traffic_live_geom%3BTunnels%3B).

## Data processing

Data processing scripts used in this work are provided in [src/data_processing/R](https://github.com/saverymax/multi-modal-pollution/tree/main/src/data_processing/R). Scripts there include the EEA data scraping script, plotting scripts to visualize the air pollutants, COVID-19, and traffic data.

Importantly, there are two scripts named station_selection_for_mvts.R and save_data_for_mvts.R. If you want to reproduce our work with the transformer and regenerate the training/testing data we use, first run station_selection_for_mvts.R and then save_data_for_mvts.R. This will first select the 41 air measuring stations by criteria of complete data and then generate the training and testing data, as described in the thesis work associated with this project. This, of course, requires the data scraping to be done first, or to download the OSF data (which is the product of the scraping script).
