"""
Module to collect data on COVID-19 occurrence
Infection rate, deaths, etc.

For BE, there is https://epistat.wiv-isp.be/covid/
The specific data set was located at https://epistat.sciensano.be/Data/COVID19BE.xlsx

The purpose of this script is to create one dataframe with all COVID variables of interest:
Cases, hospitalizations, mortality, tests, and vaccination when relevant.

The xlsx provides a time series for all Belgian cities begininning in March, 2020,
up until the present day (9-24-2021) when downloaded

TODO: Include normalizing constant of population.
"""

import pandas as pd

def process_cases(df):
    """
    Process case data

    Have to deal with multiple arrondissement in Brx
    """
    processed_df = df[['DATE', 'CASES']]
    processed_df['CASES'].replace('<5', '4')
    process_df = processed_df.groupby(['DATE']).mean()
    return processed_df


def process_data():
    """
    Read and process original xlsx
    """
    # Not using CASES_AGESEX variable because not all age groups and genders
    # available for each day.
    sheet_names = ['CASES_MUNI']
    #sheet_names = ['CASES_MUNI', 'HOSP', 'MORT', 'TESTS', 'VACC']

    #covid_df = pd.read_excel("../data/covid/COVID19BE.xlsx", sheet_name='HOSP')
    covid_df = pd.read_excel("../data/covid/COVID19BE.xlsx", sheet_name=sheet_names)
    # Create empty dict for each processed dataset of each variable, to be merged
    processed_dfs = {}
    min_date = "2020-03-01"
    for sheet in sheet_names:
        # Region or province doesn't matter for Brx,
        brx_subset = covid_df[sheet].loc[covid_df[sheet]['REGION'] == "Brussels"]
        print(len(brx_subset['DATE']))
        if sheet == 'CASES_MUNI':
            processed_dfs['sheet'] = process_cases(brx_subset)
    print(processed_dfs)


process_data()
