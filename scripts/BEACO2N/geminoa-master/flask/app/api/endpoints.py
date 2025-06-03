from . import api
from .errors import forbidden

from flask import jsonify

from ..models import BeaconReading, BeaconReadingType


# List all the devices
# Add device
@api.route("/devices", methods=["POST"])
def get_devices():
    return forbidden("device already exists")


@api.route("/data")
def data():
    """Endpoint which returns jsonified data"""

    colors = [
        "#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf",
    ]

    # Fetch all Readings
    datasets = []
    for idx, key in enumerate(BeaconReadingType):
        readings = (
            BeaconReading.query.filter_by(reading_type=key)
            .order_by(BeaconReading.sensor_timestamp.desc())
            .limit(1000)
            .all()
        )

        dataset = {
            "title": f"{key.value}".title(),
            "data": [dict(x=r.sensor_timestamp, y=r.value) for r in readings],
            "color": colors[idx % 10],
        }
        datasets.append(dataset)

    return jsonify({"datasets": datasets})
