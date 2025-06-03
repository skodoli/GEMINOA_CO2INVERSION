# How to export data

The [BEACO2N map page](http://beacon.berkeley.edu/map/) is the easiest way to access the available data.

Individual nodes (the air sensors) can be entered into the box on the left or groups of nodes (i.e. all of Glasgow) can be selected under the “select group” dropdown. The time range can be adjusted and one or multiple variables can be selected. The “60” is preselected so that the data is presented in 60-minute averages, but that can be changed as needed. We tend to work with the 60-minute data most often. Once everything is selected, a graph of the data auto-generates, and there are options to download the selected data as a CSV below the graph.

The pollutants measured have different names depending on what calibrations/corrections have been applied. Here are the most-often used raw and calibrated data variable names:

- CO2: `CO2_raw` (raw) and `co2_corrected_avg_t_drift_applied` (calibrated and quality-checked)
- CO: `co_wrk_aux` (raw) and `CO_corrected` (calibrated and quality-checked)
- O3: `o3_wrk_aux` (raw) and `o3_corrected_high_t_applied` (calibrated)
- PM (shinyei): `pm_pct_fs` (raw) and `pm_pct_fs_corrected` (calibrated and quality-checked)
- PM (PlanTower): `pm2_5` (raw) and `pm_pt_corrected` (calibrated and quality-checked)
- NO: `no_wrk_aux` (raw)
- NO2: `no2_wrk_aux`(raw)
- Temperature: `temp`
- Pressure: `pressure`
- Relative Humidity: `Rh`
