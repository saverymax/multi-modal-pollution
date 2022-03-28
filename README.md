# multi-modal-pollution
Utilizing disparate data sources, train a transformer to forecast air pollution levels during COVID-19 lockdowns in Brussels

# DATA

## EEA Air pollution

The data can be downloaded from https://eur-lex.europa.eu/eli/dir/2008/50/2015-0
9-18 using the air quality measurement stations as mandated by Directive
2008/50/EC of the European Parliament.

## Sentinel-5p

GeoJson file for Brussels generated using the tool http://geojson.io/#map=11/50.8053/4.2668. We selected a rectangle that contained the majority of the tunnels that we include in our analysis.

The tunnels of interest can be found at https://data.mobility.brussels/mobigis/?x=485351&y=6593040&zoom=12&baselayer=urbis_grey&layers=traffic_live_geom%3BTunnels%3B. We selected those that were at main entry points to the city.

Run from https://github.com/bilelomrani1/s5p-tools

analysis: https://github.com/bilelomrani1/s5p-analysis

## Traffic Counts
Data provided by Bruxelles Mobilite upon request.

## COVID-19

Brussels COVID-19 data is available at https://epistat.sciensano.be/Data/COVID19BE.xlsx. The description of the variables can be found in https://epistat.sciensano.be/COVID19BE_codebook.pdf and other dataset information at https://epistat.wiv-isp.be/covid/.

# Models

The transformer code can be found in the repository https://github.com/saverymax/mvts_transformer, forked from https://github.com/gzerveas/mvts_transformer.
