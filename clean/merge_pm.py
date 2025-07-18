import pandas as pd
import os
from glob import glob

quantaq_dir = './data/quantaq'
beaco2n_dir = './data/beaco2n'

# Read and format QuantAQ CSVs
quantaq_dfs = []
for filepath in glob(os.path.join(quantaq_dir, '*.csv')):
    location = os.path.splitext(os.path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=['period_end_utc'])
    df = df[['period_end_utc', 'pm25', 'temp', 'rh']].copy()
    df.rename(columns={'period_end_utc': 'timestamp'}, inplace=True)
    df['sensor'] = 'quantaq'
    df['location'] = location
    df['timestamp'] = pd.to_datetime(df['timestamp'], utc=True).dt.round("h")
    quantaq_dfs.append(df)

# Read and format BEACO2N CSVs
beaco2n_dfs = []
for filepath in glob(os.path.join(beaco2n_dir, '*.csv')):
    location = os.path.splitext(os.path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=['datetime'])
    df = df[['datetime', 'pm2_5', 'temp', 'rh']].copy()
    df.rename(columns={'datetime': 'timestamp', 'pm2_5': 'pm25'}, inplace=True)
    df['sensor'] = 'beaco2n'
    df['location'] = location
    df['timestamp'] = pd.to_datetime(df['timestamp'], utc=True).dt.round("h")
    beaco2n_dfs.append(df)

combined_df = pd.concat(quantaq_dfs + beaco2n_dfs)

long_df = pd.melt(
    combined_df,
    id_vars=['timestamp','sensor','location'],
    value_vars=['pm25','temp','rh'],
    var_name='parameter',
    value_name='value'
)
wide_df = pd.pivot(
    long_df,
    index='timestamp',
    columns=['parameter','sensor','location'],
    values='value'
)
wide_df.columns = [
    f"{param}_{sensor}_{loc}"
    for param, sensor, loc in wide_df.columns
]
wide_df.reset_index(inplace=True)
wide_df.dropna(how='all',subset=[col for col in wide_df.columns if col != 'timestamp'], inplace=True)
wide_df.rename(inplace=True, columns={'timestamp':'date'})
wide_df.to_csv('./clean_data/merged_pm.csv', index=False)