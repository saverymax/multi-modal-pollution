"""
Module to collect traffic counts

In Brx, the API is at https://data.mobility.brussels/traffic/api/counts/
"""

import requests
import json

def query_tomtom():
    """
    Query the tomtom api for historical traffic data
    """
    pass

def download():
    """
    Query the API
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
    traffic = download()
    file_name = "brx_traffic_counts.json"
    with open("../data/traffic/{}".format(file_name), "w", encoding="utf-8") as f:
        json.dump(traffic, f)


if __name__ == "__main__":
    #args = get_args().parse_args()
    run_processing()
