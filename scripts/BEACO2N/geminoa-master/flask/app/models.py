from . import db
from datetime import datetime
from enum import Enum

# import Choice type from flask sqlalchemy utils


class BeaconDevice(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    berkeley_id = db.Column(db.Integer, unique=True, nullable=False)
    sensor_id = db.Column(db.String, unique=True, nullable=False)
    location = db.Column(db.String, unique=False, nullable=True)
    area = db.Column(db.String, unique=False, nullable=True)
    postcode = db.Column(db.String, unique=False, nullable=True)
    latitude = db.Column(db.Float, unique=False, nullable=True)
    longitude = db.Column(db.Float, unique=False, nullable=True)
    deployment_batch = db.Column(db.Integer, unique=False, nullable=True)
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now, onupdate=datetime.now)

    def __repr__(self):
        # print device_id and last seen
        return f"<Device {self.sensor_id} last seen {self.created_at}>"


class BeaconDeviceConfig(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    device = db.Column(db.Integer, db.ForeignKey("beacon_device.id"), nullable=False)
    sync_frequency = db.Column(db.Boolean, nullable=False, default=False)
    sim_number = db.Column(db.String, nullable=True)
    imei = db.Column(db.String, nullable=True)


class BeaconReadingType(Enum):
    CO2_CORRECTED = "co2_corrected_avg_t_drift_applied"
    CO_WRK_AUX = "co_wrk_aux"
    CO_CORRECTED = "co_corrected"
    NO2_WRK_AUX = "no2_wrk_aux"
    NO2_CORRECTED = "no2_corrected"
    NO_WRK_AUX = "no_wrk_aux"
    NO_CORRECTED = "no_corrected"
    O3_WRK_AUX = "o3_wrk_aux"
    O3_CORRECTED = "o3_corrected"
    PM1 = "pm1"
    PM2_5 = "pm2_5"
    PM_PT_CORRECTED = "pm_pt_corrected"
    PM10 = "pm10"
    PRESSURE = "pressure"
    RELATIVE_HUMIDITY = "relative_humidity"
    TEMPERATURE = "temperature"

    @classmethod
    def is_in_enum(cls, value):
        return any(value == item.value for item in cls)


class BeaconReading(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    device = db.Column(db.Integer, db.ForeignKey("beacon_device.id"), nullable=False)
    sensor_timestamp = db.Column(db.DateTime, nullable=False)
    server_timestamp = db.Column(db.DateTime, nullable=False)
    node_file_id = db.Column(db.Integer, nullable=True)
    reading_type = db.Column(db.Enum(BeaconReadingType), nullable=False)
    value = db.Column(db.Float, nullable=False)
    unit = db.Column(db.String, nullable=True)
