"""
small script to explore geosjon structure
"""
import json

path = "/home/timor/Documents/thesis/code/multi-modal-pollution/data/Brussels_reduced_polygon.json"
with open(path, "r", encoding="utf8") as f:
    data = json.load(f)

coordinates = data["geometry"]["coordinates"]
print(len(coordinates))

for c in coordinates[0]:
    print(c)

    
        

