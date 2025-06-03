import click

from . import bp
from ..models import BeaconDevice, BeaconDeviceConfig, BeaconReading, BeaconReadingType
from .. import db

import pandas as pd
import numpy as np
from tqdm import tqdm


@bp.cli.command("load-devices", help="Load devices from a CSV file")
@click.argument("csvfile")
def load_devices(csvfile):
    devices = pd.read_csv(csvfile)
    devices = devices.fillna(np.nan).replace([np.nan], [None])
    for _, row in tqdm(devices.iterrows(), total=len(devices)):
        d = BeaconDevice(
            berkeley_id=row["berkeley_node"],
            sensor_id=row["sensor"],
            location=row["location"],
            area=row["area"],
            postcode=row["postcode"],
            latitude=row["latitude"],
            longitude=row["longitude"],
            deployment_batch=row["Sensor batch"],
        )
        db.session.add(d)
        db.session.commit()

        db_conf = BeaconDeviceConfig(
            device=d.id,
            sync_frequency=row["Fixed sync frequency?"] == "x",
            sim_number=row["sim_number"],
        )

        db.session.add(db_conf)
        db.session.commit()


@bp.cli.command("load-readings", help="Load readings from a CSV file")
@click.argument("csvfile")
def load_readings(csvfile):
    readings = pd.read_csv(csvfile, parse_dates=["local_timestamp", "datetime"])
    readings = readings.fillna(np.nan).replace([np.nan], [None])
    readings.rename(
        columns={"rh": "relative_humidity", "temp": "temperature"}, inplace=True
    )

    first_row = readings.iloc[0]
    d = BeaconDevice.query.filter_by(berkeley_id=int(first_row["node_id"])).first()
    if d is None:
        print(f"Device {first_row['sensor']} not found")
        return

    for idx, row in tqdm(readings.iterrows(), total=len(readings)):
        # This is the Berkeley id which is stored at column `berkeley_id`
        # row = row.to_dict()  # need this to get primative types?
        for column, value in tqdm(row.items(), total=len(row), leave=False):
            if BeaconReadingType.is_in_enum(column) and value is not None:
                r = BeaconReading(
                    device=d.id,
                    sensor_timestamp=row["datetime"],
                    server_timestamp=row["local_timestamp"],
                    reading_type=BeaconReadingType(column),
                    value=value,
                    unit=None,
                )
                db.session.add(r)
    db.session.commit()
