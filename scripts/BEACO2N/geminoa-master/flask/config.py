import os

basedir = os.path.abspath(os.path.dirname(__file__))


# Configuration
class Config:
    DEBUG = False
    TESTING = False
    CSRF_ENABLED = True
    SECRET_KEY = os.environ.get("SECRET_KEY") or "hard to guess string"
    USERNAME = "admin"
    PASSWORD = "pass"
    ADMINS = ["christos.tachtatzis@strath.ac.uk"]
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    BOOTSTRAP_SERVE_LOCAL = True

    @staticmethod
    def init_app(app):
        pass


class DevelopmentConfig(Config):
    DEBUG = True
    # Use the following for SQLite
    SQLALCHEMY_DATABASE_URI = "sqlite:///" + os.path.join(
        os.path.join(basedir, "db"), "app.db"
    )
    # SQLALCHEMY_DATABASE_URI = "postgresql://username:password@localhost/geminioa_dev"
    SQLALCHEMY_ECHO = False


class TestingConfig(Config):
    TESTING = True
    SQLALCHEMY_DATABASE_URI = "postgresql://username:password@localhost/geminoa_test"


class ProductionConfig(Config):
    DEBUG = False
    TESTING = False
    SQLALCHEMY_DATABASE_URI = (
        os.getenv("SQLALCHEMY_DATABASE_URI")
        or "postgresql://username:password@localhost/geminoa_production"
    )


config = {
    "development": DevelopmentConfig,
    "testing": TestingConfig,
    "production": ProductionConfig,
    "default": DevelopmentConfig,
}
