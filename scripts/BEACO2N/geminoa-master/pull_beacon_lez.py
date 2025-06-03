#!/usr/bin/env python3
import requests
import datetime
from pathlib import Path

sensors = [
    (194, "S4", "University of Strathclyde Sports Centre"),
    (171, "S17", "University of Strathclyde Royal college")
]

variables = [
    
    "co2_corrected_avg","co2_corrected_stdev"
]

# "co2_corrected_avg_t_drift_applied", "co2_raw", "co_corrected", "co_wrk_aux", "no2_wrk_aux", "no_wrk_aux", "o3_corrected_high_t_applied", "o3_wrk_aux", "pm2_5", "pm_pct_fs", "pm_pct_fs_corrected", "pm_pt_corrected", "pressure", "rh", "temp"
variablestr = ",".join(variables)

start = "2022-08-31 17:00:00".replace(" ", "%20")
#end = datetime.datetime.now().strftime("%F%%20%T")

end = "2022-10-31 17:00:00".replace(" ", "%20")

outdir = Path("./data/beacon/sepoct22_lez").resolve()
outdir.mkdir(exist_ok=True)

def get_data(db_id, glasgow_sid, sensorname):
    sensor_sanitised = sensorname.replace(" ", "%20")
    url = f"http://beacon.berkeley.edu/node/{db_id}/measurements_all/csv?name={sensorname}&interval=60&variables={variablestr}&start={start}&end={end}&chart_type=measurement"
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
