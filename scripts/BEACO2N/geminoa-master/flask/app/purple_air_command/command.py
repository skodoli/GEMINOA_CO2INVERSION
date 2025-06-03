import click

from . import bp


@bp.cli.command("fetch", help="Fetch from PurpleAir API")
@click.argument("endpoint")
def load_devices(endpoint):
    raise NotImplementedError("Not implemented yet")
