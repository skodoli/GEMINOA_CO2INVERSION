#!/usr/bin/env python
import requests
from pathlib import Path
from dateutil.relativedelta import relativedelta
from dateutil.parser import parse as dateparse
import sys

sensors = [
    (170, "All Saints Catholic Secondary School"),
    (163, "St Cuthbert’s Primary School"),
    (195, "Glasgow S5"),
    (198, "Glasgow S8"),
    (167, "Glasgow S13"),
    (173, "Glasgow S19"),
    (177, "Glasgow S23"),
    (180, "Glasgow S26"),
    (181, "Glasgow S27"),
    (182, "Glasgow S28"),
    (183, "Glasgow S29"),
    (184, "Glasgow S30"),
    (185, "Glasgow S31"),
    (187, "Glasgow S32"),
    (188, "Glasgow S33"),
    (189, "Glasgow S34"),
    (191, "Glasgow S36"),
]

variables = [
    "co2_raw",
    "co2_corrected_avg_t_drift_applied",
    "pm_pt_corrected",
    "pressure",
    "rh",
    "temp",
]
# "co2_corrected_avg_t_drift_applied", "co2_raw", "co_corrected", "co_wrk_aux", "no2_wrk_aux", "no_wrk_aux", "o3_corrected_high_t_applied", "o3_wrk_aux", "pm2_5", "pm_pct_fs", "pm_pct_fs_corrected", "pm_pt_corrected", "pressure", "rh", "temp"
variablestr = ",".join(variables)


def date_range(start, end):
    # generate a list of dates between start and end
    # iterating by month
    start = dateparse(start)
    end = dateparse(end)
    while start < end:
        yield start, start + relativedelta(months=1)
        start += relativedelta(months=1)

for start, end in date_range("2021-08-22 11:00:00", "2023-06-12 11:00:00"):
    # start = "2021-08-22 11:00:00".replace(" ", "%20")
    # end = "2021-09-12 11:00:00".replace(" ", "%20")
    start = str(start)
    end = str(end)
    start_date = start.split("%20")[0]
    end_date = end.split("%20")[0]

    outdir = Path("./beacon-problem-data").resolve()
    outdir.mkdir(exist_ok=True)

    for sid, sensorname in sensors:
        sensor_sanitised = sensorname.replace(" ", "%20")
        url = f"http://beacon.berkeley.edu/node/{sid}/measurements_all/csv?name={sensorname}&interval=60&variables={variablestr}&start={start}&end={end}&chart_type=measurement"
        resp = requests.get(url)
        csv = resp.text
        print(sensorname, start, len(csv) / 1024, "kb")
        outfile = outdir / f"{sensorname}--{start_date}-{end_date}.csv"
        outfile.write_text(csv)
