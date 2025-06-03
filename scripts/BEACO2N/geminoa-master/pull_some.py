#!/usr/bin/env python3
import requests
import datetime
from pathlib import Path

sensors = [
    
    (192, "S2", "Pirie Park Primary School"),
    (197, "S7", "Notre Dame High School"),
    (168, "S14", "Haghill Primary School"),
    (171, "S17", "University of Strathclyde Royal college"),  
    (175, "S21", "Bellahouston Academy"),
    (176, "S22", "Ashpark Primary School"),
    (178, "S24", "St Thomas Aquinas R C Secondary School"),
    (179, "S25", "John Paul Academy")
]

variables = [
    
    "pm_pt_corrected"
]

# "co2_corrected_avg_t_drift_applied", "co2_raw", "co_corrected", "co_wrk_aux", "no2_wrk_aux", "no_wrk_aux", "o3_corrected_high_t_applied", "o3_wrk_aux", "pm2_5", "pm_pct_fs", "pm_pct_fs_corrected", "pm_pt_corrected", "pressure", "rh", "temp"
variablestr = ",".join(variables)

start = "2022-01-01 00:00:00".replace(" ", "%20")
#end = datetime.datetime.now().strftime("%F%%20%T")

end = "2022-12-31 23:00:00".replace(" ", "%20")

outdir = Path("./PM").resolve()
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
