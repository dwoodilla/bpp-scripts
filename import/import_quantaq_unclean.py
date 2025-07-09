import requests

# See QuantAQ API docs here: https://docs.quant-aq.com/software-apis-and-libraries/quantaq-cloud-api

# Token: K8YZ1BRT2U6SX8AQCNGAW991
# Breathe PVD org code: 2521

# TODO: Implement multithreading; this is very slow.

with requests.Session() as s:
    s.auth = ("K8YZ1BRT2U6SX8AQCNGAW991","")
    devices:list = s.get("https://api.quant-aq.com/v1/orgs/2521").json()["devices"]

    device_data = {device : list() for device in devices}

    for device in devices:
        next_url = f"https://api.quant-aq.com/v1/devices/{device}/data/?page=1&per_page=1000"
        while next_url!=None:
            resp = s.get(next_url)
            resp_json = resp.json()
            if resp.status_code!=200:
                raise RuntimeError(f"Error with initial data request for sn={device}: status_code={resp.status_code}")
            device_data[device].append(resp_json)
            next_url = resp_json["meta"]["next_url"]
            print(f"next url:{next_url}")
    print(device_data)

        
