from flask import render_template, send_from_directory
from . import main
from ..models import BeaconDevice


@main.route("/")
def index():
    """Route which renders the chart page.  Pass the endpoint which returns jsonified data"""
    return render_template("index.html")


# List devices
@main.route("/devices")
def devices():
    devices = BeaconDevice.query.all()
    return render_template("main/devices.html", devices=devices)


@main.route("/favicon.ico")
def favicon():
    return send_from_directory(
        "static", "favicon.ico", mimetype="image/vnd.microsoft.icon"
    )
