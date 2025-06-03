from flask import Blueprint

bp = Blueprint("beacon", __name__)
bp.cli.short_help = "Load/Save beacon data commands"

from . import command
