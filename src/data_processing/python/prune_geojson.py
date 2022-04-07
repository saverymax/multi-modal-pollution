"""
script to semi-automatically reduce the size of the urban polygons.

python prune_geojson.py --path /home/timor/Documents/thesis/code/multi-modal-pollution/data/Brussels_reduced_polygon.json
"""
import json
import argparse


def get_args():
    parser = argparse.ArgumentParser(description="Arguments for pruning json")
    parser.add_argument("--path", dest="json_file", help="Path to geosjson to prune")

return parser

args = get_args().parse_args()

brx_coord = [
    [4.258451519563525, 50.914279082955005],
    [4.514226970172459, 50.91362970159218],
    [4.512853679162479, 50.798980414559665],
    [4.251241741761125, 50.795074345447155]
    ] 

path =
with open(path, "r", encoding="utf8") as f:
    data = json.load(f)

coordinates = data["geometry"]["coordinates"]
print(len(coordinates))

for c in coordinates[0]:
    print(c)
