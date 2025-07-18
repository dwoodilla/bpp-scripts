import pandas as pd
import os
from glob import glob
import pyaqsapi as aqs
from datetime import date
from functools import reduce

bcn_files = glob("../data/beaco2n/*.csv")
qaq_files = glob("../data/quantaq/*.csv")
aqs_files = glob("../data/aqs/*.csv")

bcn_df = {os.path.splitext(os.path.basename(f))[0] : pd.read_csv(f) for f in bcn_files}
qaq_df = {os.path.splitext(os.path.basename(f))[0] : pd.read_csv(f) for f in qaq_files}
aqs_df = {os.path.splitext(os.path.basename(f))[0] : pd.read_csv(f) for f in aqs_files}

def clean_bcn(df : pd.DataFrame)-> pd.DataFrame:
    df.rename(columns={"datetime":"date", "pm2_5":"pm25"}, inplace=True)
    df["date"] = pd.to_datetime(df["date"], utc=True).dt.round("h")
    df.drop(columns=["local_timestamp","epoch","node_file_id", "node_id"], inplace=True)
    df.rename({"co_corrected":"co"})
    wrk_aux_cols = df.filter(regex=r"_wrk_aux$").columns
    df.rename(columns={col : col.replace("_wrk_aux", "_raw") for col in wrk_aux_cols}, inplace=True)

    df = df.melt(
        id_vars=["date"],
        var_name="parameter",
        value_name="value"
    )
    df = df.assign(corrected=lambda df_arg : ~df_arg["parameter"].str.endswith("_raw")) # '~' is bitwise NOT
    df["parameter"] = df["parameter"].str.removesuffix("_raw").str.removesuffix("_corrected")
    df.dropna(how="all", subset="value", inplace=True)
    return df

def clean_qaq(df :pd.DataFrame)-> pd.DataFrame :
    df.rename(columns={"period_start_utc":"date"}, inplace=True)
    df["date"] = pd.to_datetime(df["date"], utc=True)
    df.drop(columns=["period_start","period_end","period_end_utc","sn","n_datapoints"], inplace=True)

    df = df.melt(
        id_vars=["date"],
        var_name="parameter",
        value_name="value"
    )
    df.dropna(how="all", subset="value", inplace=True)
    df = df.assign(corrected=lambda df_arg : True)
    return df
    

bcn_df = {site : clean_bcn(df) for site, df in bcn_df.items()}
qaq_df = {site : clean_qaq(df) for site, df in qaq_df.items()}
print(qaq_df["dpw"].head())