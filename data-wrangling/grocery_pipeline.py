import numpy as np
import pandas as pd
import re
import sys
import os

# Load the data
def load_data(csv_name):
    """Load data from csv file"""
    return pd.read_csv(csv_name)

# Get the latitude and longitude from the location column
def get_lat_long(string):
    lat = re.search(r"'latitude': '([-]?\d+\.\d+)'", string)
    lat = lat.group(1) if lat else None

    lon = re.search(r"'longitude': '([-]?\d+\.\d+)'", string)
    lon = lon.group(1) if lon else None

    return lat, lon

# Clean the data
def clean_data(df):
    # Get all of the data where the county is New York
    df = df[df['County'] == 'New York']

    # Reset the index
    df = df.reset_index(drop=True)
    
    # Remove all of the rows where the location is null
    df = df[df['Location'].notnull()]

    # Get all of the longitude and latitude data into separate column
    # Latitude is the first dictionary value and longitude is the second in the Location column
    # Since the location column is a string, we need to regex to get the values out
    df['Latitude'] = np.nan
    df['Longitude'] = np.nan
    for i, row in df.iterrows():
        lat, lon = get_lat_long(row['Location'])
        
        if lat is not None and lon is not None:
            # Append the lat value to the Latitude column
            df.loc[i, 'Latitude'] = float(lat)
            # Append the lon value to the Longitude column
            df.loc[i, 'Longitude'] = float(lon)
    
    # Establishment type must contain the letter 'O' or 'R' or 'D' or 'G' or 'T' or 'H'
    df = df[df['Establishment Type'].str.contains('O|R|D|G|T|H')]

    return df

df = load_data('data/retail-food-stores.csv')
df = clean_data(df)

print(df)

# Save the data
df.to_csv('cleaned-data/retail-food-stores-cleaned.csv', index=False)
