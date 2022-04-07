# DATA
All data except Sentinel-5P data can be found the the data repository. The Sentinel-5P data must be downloaded and processed yourself, though there may be a shared drive made available for this data.

## EEA Air pollution

The data can be downloaded from https://eur-lex.europa.eu/eli/dir/2008/50/2015-0
9-18 using the air quality measurement stations as mandated by Directive
2008/50/EC of the European Parliament.

## COVID-19

Belgium COVID-19 data is available at https://epistat.sciensano.be/Data/COVID19BE.xlsx. The description of the variables can be found in https://epistat.sciensano.be/COVID19BE_codebook.pdf and other dataset information at https://epistat.wiv-isp.be/covid/.

## Traffic Counts
Data provided by Bruxelles Mobilite upon request. 

The tunnels of interest can be found at https://data.mobility.brussels/mobigis/?x=485351&y=6593040&zoom=12&baselayer=urbis_grey&layers=traffic_live_geom%3BTunnels%3B. We selected those that were at main entry points to the city. See the ARIMA R scripts for processing of the data.

## Sentinel-5p

GeoJson file for Brussels generated using the tool http://geojson.io/#map=11/50.8053/4.2668. We selected a rectangular polygon that contained the majority of the tunnels included in our analysis.

Run from https://github.com/bilelomrani1/s5p-tools. Analytic tools available at https://github.com/bilelomrani1/s5p-analysis


