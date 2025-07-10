import pandas as pd
import os
from glob import glob

quantaq_dir = './data/quantaq'
beaco2n_dir = './data/beaco2n'

# Read and format Quantaq CSVs
quantaq_dfs = []
for filepath in glob(os.path.join(quantaq_dir, '*.csv')):
    location = os.path.splitext(os.path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=['period_end_utc'])
    df = df[['period_end_utc', 'pm25']].copy()
    df.rename(columns={'period_end_utc': 'timestamp'}, inplace=True)
    df['sensor'] = 'quantaq'
    df['location'] = location
    df['timestamp'] = pd.to_datetime(df['timestamp'], utc=True).dt.round("h")
    quantaq_dfs.append(df)

# Read and format Beaco2n CSVs
beaco2n_dfs = []
for filepath in glob(os.path.join(beaco2n_dir, '*.csv')):
    location = os.path.splitext(os.path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=['datetime'])
    df = df[['datetime', 'pm2_5']].copy()
    df.rename(columns={'datetime': 'timestamp', 'pm2_5': 'pm25'}, inplace=True)
    df['sensor'] = 'beaco2n'
    df['location'] = location
    df['timestamp'] = pd.to_datetime(df['timestamp'], utc=True).dt.round("h")
    beaco2n_dfs.append(df)

# Combine all data
all_dfs = quantaq_dfs + beaco2n_dfs
combined_df = pd.concat(all_dfs)

# Pivot to wide format: one column per species+sensor+location
combined_df['param_col'] = combined_df.apply(
    lambda row: f"{row['species'] if 'species' in row else 'pm25'}_{row['sensor']}_{row['location']}", axis=1
)
wide_df = combined_df.pivot_table(
    index='timestamp',
    columns='param_col',
    values='pm25',
    aggfunc='mean'  # use mean in case of duplicate timestamps
).reset_index()

wide_df.to_csv('./clean_data/merged_pm.csv', index=False)
