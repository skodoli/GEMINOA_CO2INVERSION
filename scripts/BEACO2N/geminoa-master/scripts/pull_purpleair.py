#!/usr/bin/env python3
import requests
import datetime
from pathlib import Path

sensors = []

fields = [
    # station information and status fields
    "name",
    "model",
    "latitude",
    "longitude",
    "altitude",
    "uptime",
    # environmental
    "humidity",
    "temperature",
    "pressure",
    # miscellaneous
    "voc",
    "ozone1",
    "time_stamp",
    # pm
    "pm1.0",
    "pm2.5",
    "pm10.0",
    # particle count
    "0.3_um_count",
    "0.5_um_count",
    "1.0_um_count",
    "2.5_um_count",
    "5.0_um_count",
    "10.0_um_count",
]

start = "2024-01-01 01:00:00".replace(" ", "%20")
end = "2024-01-01 02:00:00".replace(" ", "%20")
# end = datetime.datetime.now().strftime("%F%%20%T")

# end = "2023-06-11 11:00:00".replace(" ", "%20")

outdir = Path("./purpleair").resolve()
outdir.mkdir(exist_ok=True)


def get_data(sid):
    url = f"https://api.purpleair.com/v1/sensors/:{sid}"

    sensor_sanitised = sensorname.replace(" ", "%20")
    url = f"http://beacon.berkeley.edu/node/{sid}/measurements_all/csv?name={sensorname}&interval=60&variables={variablestr}&start={start}&end={end}&chart_type=measurement"
    resp = requests.get(url)
    csv = resp.text
    print(f"{sensorname:<40}\t{len(csv) / 1024:.2} kb")
    outfile = outdir / f"{sensorname}.csv"
    outfile.write_text(csv)


if __name__ == "__main__":
    # create multiprocess pool
    import multiprocessing

    print(f"{start = :}")
    print(f"{end = :}")
    num_processes = len(sensors)
    with multiprocessing.Pool(num_processes) as p:
        p.starmap(get_data, sensors)
