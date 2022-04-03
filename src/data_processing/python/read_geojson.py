import json
# can also use geopandas

def read_json(path):
    """Parse GeoJson and query some features"""
    with open(path, "r", encoding="utf8") as f:
        data = json.load(f)
    print(len(data))
    cities = ["Paris", "Brussels", "Amsterdam", "Berlin"]
    for entry in data['features']:
        city = entry['properties']['name_conve']
        if city in cities:
            with open("C:\\Users\\Timor\\Documents\\ku_leuven\\thesis\\data\\{}_polygon.json".format(city), "w", encoding="utf8") as f:
                json.dump(entry, f)

path = "C:\\Users\\Timor\\Documents\\ku_leuven\\thesis\\data\\stanford-yk247bg4748-geojson.json"
read_json(path)
