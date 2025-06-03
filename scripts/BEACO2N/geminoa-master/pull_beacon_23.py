#!/usr/bin/env python3
import requests
import datetime
from pathlib import Path

sensors = [
    
    (192, "S2", "Pirie Park Primary School"),
    (193, "S3", "Knightswood Secondary"),
    (194, "S4", "University of Strathclyde Sports Centre"),
    (196, "S6", "Eastbank Academy"),
    (197, "S7", "Notre Dame High School"),
    (163, "S10", "St Cuthbert’s Primary School"),
    (166, "S12", "St Maria Goretti Primary School"),
    (168, "S14", "Haghill Primary School"),
    (169, "S15", "Riverside Primary School"),
    (170, "S16", "All Saints Catholic Secondary School"),
    (171, "S17", "University of Strathclyde Royal college"),  # Sports centre or RCB?
    (172, "S18", "St Pauls High School"),
    (174, "S20", "Hillpark Secondary School"),
    (175, "S21", "Bellahouston Academy"),
    (176, "S22", "Ashpark Primary School"),
    (177, "S23", "King's Park Secondary"),
    (178, "S24", "St Thomas Aquinas R C Secondary School"),
    (179, "S25", "John Paul Academy"),
    (170, "All Saints Catholic Secondary School"),
    (163, "St Cuthbert’s Primary School")
]

variables = [
    
    "co2_corrected_avg","co2_corrected_stdev"
]

# "co2_corrected_avg_t_drift_applied", "co2_raw", "co_corrected", "co_wrk_aux", "no2_wrk_aux", "no_wrk_aux", "o3_corrected_high_t_applied", "o3_wrk_aux", "pm2_5", "pm_pct_fs", "pm_pct_fs_corrected", "pm_pt_corrected", "pressure", "rh", "temp"
variablestr = ",".join(variables)

start = "2023-12-01 00:00:00".replace(" ", "%20")
#end = datetime.datetime.now().strftime("%F%%20%T")

end = "2023-12-31 23:00:00".replace(" ", "%20")

outdir = Path("./data/beacon/dec23").resolve()
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
