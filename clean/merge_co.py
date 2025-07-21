import pandas as pd
from os import path
from glob import glob

aqs_dir = "./data/aqs"
quantaq_dir = "./data/quantaq"
beaco2n_dir = "./data/beaco2n"


aqs_dfs = []
for filepath in glob(path.join(aqs_dir, "*.csv")):
    location = path.splitext(path.basename(filepath))[0]
    df = pd.read_csv(filepath)
    df = df[["date_gmt","time_gmt","sample_measurement"]].copy()
    df["date"] = pd.to_datetime(df["date_gmt"].astype(str) + " " + df["time_gmt"].astype(str), utc=True).dt.round("h")
    df = df.drop(columns=["date_gmt","time_gmt"])
    df["sensor"] = "aqs"
    df["location"] = location
    df = df.rename(columns={"sample_measurement":"co"})
    aqs_dfs.append(df)


quantaq_dfs = []
for filepath in glob(path.join(quantaq_dir, "*.csv")):
    location = path.splitext(path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=["period_end_utc"])
    df = df[["period_end_utc", "co", "temp", "rh"]].copy()
    df.rename(columns={"period_end_utc": "date"}, inplace=True)
    df["sensor"] = "quantaq"
    df["location"] = location
    df["date"] = pd.to_datetime(df["date"], utc=True).dt.round("h")
    quantaq_dfs.append(df)

beaco2n_dfs = []
for filepath in glob(path.join(beaco2n_dir, "*.csv")):
    location = path.splitext(path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=["datetime"])
    df = df[["datetime", "co_corrected", "temp", "rh"]].copy()
    df.rename(columns={"datetime": "date", "co_corrected": "co"}, inplace=True)
    df["sensor"] = "beaco2n"
    df["location"] = location
    df["date"] = pd.to_datetime(df["date"], utc=True).dt.round("h")
    beaco2n_dfs.append(df)

combined_df = pd.concat(aqs_dfs + quantaq_dfs + beaco2n_dfs)

long_df = pd.melt(
    combined_df,
    id_vars=["date","sensor","location"],
    value_vars=["co","temp","rh"],
    var_name="parameter",
    value_name="value"
)
wide_df = pd.pivot(
    long_df,
    index="date",
    columns=["parameter","sensor","location"],
    values="value"
)
wide_df = wide_df.drop(columns=[col for col in wide_df.columns if col[1]=="aqs" and col[0]!="co"])
wide_df.columns = [
    f"{param}_{sensor}_{loc}"
    for param, sensor, loc in wide_df.columns
]

wide_df = wide_df.reset_index()
wide_df = wide_df.dropna(how="all",subset=[col for col in wide_df.columns if col not in ["date","co_aqs_cranston","co_aqs_myron"]])
wide_df.to_csv("./clean_data/merged_co.csv", index=False)