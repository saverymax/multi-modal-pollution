"""
Process Brx traffic count data as supplied by Brussels Mobilite
"""

import pandas as pd

def process_data():
    """
    Read file and prepare
    """
    path = "../data/traffic/20210921-Tunnels2019-sept2021.xlsb"
    traffic_df = pd.read_excel(path)
    traffic_df = traffic_df[['From', 'Detector', 'Volume', 'Occupancy', 'Speed']]
    return traffic_df


def summarize_traffic(traffic_df):
    """
    Compute summary statistics and data exploration
    """
    print(traffic_df.groupby('Detector')['Volume', 'Occupancy', 'Speed'].describe())


def run_processing():
    """
    Main function to run processing of traffic dataset
    """
    traffic_df = process_data()
    summarize_traffic(traffic_df)

if __name__ == "__main__":
    run_processing()
