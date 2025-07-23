import pandas as pd
from os import path
from glob import glob

meas_dir = "./data/berkeley_beaco2n"
ref_dir = "./data/berkeley_beaco2n/ref"

meas_dfs = []
for filepath in glob(path.join(meas_dir, "*.csv")):
    location = path.splitext(path.basename(filepath))[0]
    df = pd.read_csv(filepath, parse_dates=["datetime"])
    df = df[["datetime", "co_corrected", "temp", "rh"]].copy()
    df.rename(columns={"datetime": "date", "co_corrected": "co"}, inplace=True)
    df["sensor"] = "beaco2n"
    df["location"] = location
    df["date"] = pd.to_datetime(df["date"], utc=True).dt.round("h")
    meas_dfs.append(df)

ref_dfs = []

picarro = pd.read_csv(path.join(ref_dir, "picarro.csv"))
picarro = picarro[["datetime","co_sync"]]
picarro = picarro.rename(columns={"datetime":"date","co_sync":"co"})
picarro["sensor"] = "super"
picarro["location"] = "rfs"
picarro["date"] = pd.to_datetime(picarro["date"], utc=True)
ref_dfs.append(picarro)

met = pd.read_csv(path.join(ref_dir, "meteorology.csv"))
met = met[["datetime","temp","rh"]]
met = met.rename(columns={"datetime":"date"})
met["sensor"] = "super"
met["location"] = "rfs"
met["date"] = pd.to_datetime(met["date"], utc=True)
ref_dfs.append(met)

combined_df = pd.concat(meas_dfs + ref_dfs)

long_df = pd.melt(
    combined_df,
    id_vars=["date","sensor","location"],
    value_vars=["co","temp","rh"],
    var_name="parameter",
    value_name="value"
)

wide_df = pd.pivot_table(
    long_df,
    index="date",
    columns=["parameter","sensor","location"],
    values="value",
    aggfunc="first"
)
wide_df.columns = [
    f"{param}_{sensor}_{loc}"
    for param, sensor, loc in wide_df.columns
]

wide_df = wide_df.reset_index()
wide_df = wide_df.dropna(how="all")
wide_df.to_csv("./clean_data/merged_berkeley_co.csv", index=False)