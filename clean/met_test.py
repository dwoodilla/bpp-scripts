import requests

req = requests.get(
    url="https://www.ncei.noaa.gov/access/services/data/v1",
    params={
        "dataset":"global-hourly",
        "stations":"99727899999",
        "startDate":"2020-01-01T00:00:00Z",
        "endDate":"2025-07-01T00:00:00Z",
        "dataTypes":",".join(["TMP","RH1"]),
        "format":"json",
        "includeAttributes":"true",
        "includeStationName":"true"
    }
)
print(req.status_code)
req = req.json()
print(req[0:5])