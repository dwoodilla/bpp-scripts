import time
import requests
from urllib.parse import quote
from datetime import datetime
import pytz

# Sensor names and corresponding node numbers
human_sensor_names = {
    num_id : name for num_id, name in zip(
        [250,251,252,253,254,255,256,257,258,259,260,
        261,262,263,264,265,266,267,268,269,270,
        271,272,274,276],
        ["myron","zuccolo","wecc","rocklib","silverlake","unitedway","cfs","pha","reservoir","ccri",
        "mtpleasant","carnevale","martialarts","southprovlib","ecubed","ricollege","blackstone","rochambeaulib","provcollege","prek",
        "smithhilllib","pema","rockspot","medschool","dpw"]
    )
}

numeric_sensor_names = {
    f"Sensor{str(i).zfill(2)}": num_id for i, num_id in zip(
        range(1, 26),
        [250, 254, 258, 261, 264, 267, 270, 274, 276, 251, 
         252, 255, 257, 259, 262, 263, 266, 269, 272, 253, 
         256, 260, 265, 268, 271]
    )
}

# Create URL list
base_url = "http://128.32.208.8/node/{node}/measurements_all/csv"
sensor_urls = {
    human_sensor_names[num_id] : 
        f"{base_url.format(node=num_id)}?name={sensor_num}&interval=60"
        f"&variables=temp,rh,pressure,pm2_5,co_wrk_aux,no2_wrk_aux,no_wrk_aux,o3_wrk_aux,co2_raw,"
        f"Vaisala_temp,co_corrected,co2_corrected_t_drift_applied"
         "&start={start_datetime}&end={end_datetime}"
        f"&chart_type=measurement"
    for sensor_num, num_id in numeric_sensor_names.items()
}

# Download function with retries
def download_with_retries(url:str, start_datetime:str, end_datetime:str, file_path:str, max_retries=3):
    url = url.format(start_datetime=quote(start_datetime), end_datetime=quote(end_datetime))
    
    for attempt in range(1, max_retries + 1):
        try:
            response = requests.get(url, timeout=120)
            if response.ok and len(response.content) > 1000:
                with open(file_path, 'wb') as f:
                    f.write(response.content)
                print(f"Downloaded: {file_path}")
                return
            else:
                print(f"Attempt {attempt} failed, response invalid or too small")
        except Exception as e:
            print(f"Attempt {attempt} error: {e}")
        time.sleep(2)
    print(f"Failed to download: {file_path}")

start_la = pytz.timezone("America/New_York") \
               .localize(datetime.strptime("2020-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")) \
               .astimezone(pytz.timezone("America/Los_Angeles")) \
               .strftime("%Y-%m-%d %H:%M:%S")
now_la = datetime.now(pytz.timezone("America/Los_Angeles")).strftime("%Y-%m-%d %H:%M:%S")

# Download sensor data
for human_name, url in sensor_urls.items():
    path = f"./data/beaco2n/{human_name}.csv"
    download_with_retries(
        url, 
        start_datetime=start_la, 
        end_datetime=now_la,
        file_path=path
    )