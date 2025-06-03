from flask import Blueprint

bp = Blueprint("purpleair", __name__)
bp.cli.short_help = "Load/Save PurpleAir data commands"

from . import command
