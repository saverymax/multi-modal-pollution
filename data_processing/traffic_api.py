"""
Module to collect traffic counts

In Brx, the API is at https://data.mobility.brussels/traffic/api/counts/
"""

import requests
import json

import tomtom_config

def query_tomtom():
    """
    Query the tomtom api for historical traffic data

    See documentation: https://developer.tomtom.com/traffic-stats/traffic-stats-apis/traffic-density
    """
    base_url = "api.tomtom.com"
    params = {
	    'jobName': 'brx_traffic', 
            'dateRange'={'name': 'lockdown-range', 'from': 2019-01-01, 'to': 2021-09-01},
	    'network': {
		"name": "Brx-poly",
		  "geometry" : {
		  "type": "Polygon",
		  "coordinates": [
		      [
			[19.44305, 51.75612],
			[19.44992, 51.75612],
			[19.44992, 51.75947],
			[19.44305, 51.75947],
			[19.44305, 51.75612]
		      ]
		    ],
		},
            }
	}
    incident_url = "https://<baseURL>/traffic/trafficstats/trafficdensity/<versionNumber>?key={1}".format(base_url, tomtom_config.api_key)
    


def query_brx_api():
    """
    Query the Bruxelles city API

    Currently does not supply historical data
    """
    url = "https://data.mobility.brussels/traffic/api/counts/"
    params = {'request': 'live', 'outputFormat': 'json'}
    r = requests.get(url, params=params)
    try:
        assert r.status_code == 200
    except e:
        print(r.status_code)
    traffic = r.json()
    #traffic = json.loads(r.text)
    print(type(traffic))
    return traffic
    
def run_processing():
    """
    Main function to download and process traffic data
    """
    traffic = query_brx_api()
    file_name = "brx_traffic_counts.json"
    with open("../data/traffic/{}".format(file_name), "w", encoding="utf-8") as f:
        json.dump(traffic, f)


if __name__ == "__main__":
    #args = get_args().parse_args()
    run_processing()
