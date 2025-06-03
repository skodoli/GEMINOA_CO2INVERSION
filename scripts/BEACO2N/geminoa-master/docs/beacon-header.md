# Beacon data header

The beacon sensors do not save a header when writing files, so have to read the source code to determine the fields.

    ["blank","pressure_mb","tempC","relative_humidity","dewpoint","pm10","pm2.5","pm100","o3_work_voltage","o3 aux voltage","co work voltage","co aux voltage","no work voltage","no aux voltage","no2 work voltage","no2 aux voltage","high counts","total counts","low percents","CO2 ppm","tempC","timestamp"]

There seems to be an error in the code. It lists pressure as coming *after* temperature, but the values don't seem to agree with that output. I've put the header above with pressure below temperature, as we wont have a 1000C temperature but are likely to have 1000 mb pressure.

- -999  -- blank
- 1014.31128 -- pressure
- 11.69000 -- tempC (BME sensor)
- 70.26660 -- RH
- 6.45153 -- dewpoint
- -999 -- pm10
- -999 -- pm2.5
- -999 -- pm100
- 0.32386 -- o3_w
- 0.31681 -- o3_a
- 0.59732 -- co_w
- 0.46719 -- co_a
- 0.38746 -- no_w
- 0.38691 -- no_a
- 0.32029  -- no2_w
- 0.31241 -- no2_a
- 321013320 -- high
- 321013320 -- total
- 0.00000 -- low %
- 486.7 -- co2 ppm
- 13.1 -- tempC (co2 sensor)
- 2021-11-08 11:00:00 -- timestamp
