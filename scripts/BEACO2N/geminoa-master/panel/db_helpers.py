from sqlalchemy import create_engine, text
import pandas as pd

engine = create_engine("sqlite:////Users/ctachtatzis/gitwork/beacon/flask/db/app.db")


class OptionDisplayer:
    @classmethod
    def create_list(cls, input, formatstr=None, keys_to_show=None):
        return [OptionItem(i, formatstr, keys_to_show) for i in input]


class OptionItem:
    def __init__(self, input, formatstr=None, keys_to_show=None):
        self.input = input
        self.formatstr = formatstr
        self.keys_to_show = keys_to_show

    def __repr__(self):
        inp = [v for i, v in enumerate(self.input) if i in self.keys_to_show]
        return self.formatstr.format(*inp)


def get_device_ids():
    # Use this mini class to create a tuple of ID and strings which you can

    sql_query = text(
        "SELECT DISTINCT location, sensor_id FROM beacon_device WHERE location is NOT null ORDER BY location ASC;"
    )
    with engine.connect() as conn:
        result = conn.execute(sql_query).fetchall()

        return [row for row in result]
    return None


def get_measurands(device_id):
    sql_query = text(
        f"""SELECT DISTINCT beacon_reading.reading_type FROM
                        beacon_reading INNER JOIN beacon_device
                        ON beacon_reading.device = beacon_device.id
                        WHERE beacon_device.sensor_id = "{device_id}";"""
    )
    # print(sql_query)
    with engine.connect() as conn:
        result = conn.execute(sql_query).fetchall()

        return [r[0] for r in result]
    return None


def get_readings(device_id, mesurand):

    sql_query = text(
        f"""SELECT beacon_reading.sensor_timestamp, beacon_reading.value
                     FROM beacon_reading INNER JOIN beacon_device
                        ON beacon_reading.device = beacon_device.id
                        where beacon_device.sensor_id = "{device_id}"
                        AND beacon_reading.reading_type = "{mesurand}"
                        order by beacon_reading.sensor_timestamp DESC limit 200;"""
    )
    return pd.read_sql(sql_query, engine, parse_dates=["sensor_timestamp"])
