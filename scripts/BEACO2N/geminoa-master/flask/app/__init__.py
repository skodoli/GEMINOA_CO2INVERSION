from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_migrate import Migrate
from flask_marshmallow import Marshmallow
import os

from config import config

db = SQLAlchemy()
migrate = Migrate()
ma = Marshmallow()


def create_app():
    config_name = os.getenv("FLASK_CONFIG") or "default"
    print(f"Running in {config_name} mode")
    app = Flask(__name__)
    app.config.from_object(config[config_name])
    config[config_name].init_app(app)

    db.init_app(app)
    migrate.init_app(app, db)
    ma.init_app(app)

    # attach routes and custom error pages here
    from .beacon_command import bp as beacon_command_blueprint

    app.register_blueprint(beacon_command_blueprint)

    from .purple_air_command import bp as purple_air_command_blueprint

    app.register_blueprint(purple_air_command_blueprint)

    from .main import main as main_blueprint

    app.register_blueprint(main_blueprint)

    from .api import api as api_blueprint

    app.register_blueprint(api_blueprint, url_prefix="/api/v0.1")

    return app
