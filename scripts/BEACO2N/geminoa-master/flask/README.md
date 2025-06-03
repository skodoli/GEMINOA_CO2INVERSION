# Flask App

## Create Environment and Install Packages

Create a new environment in anaconda or micromamba. In the following command micromamba is used but you can replace the call to `micromaba` to `conda`. The environment is assumed to be `beacon` change this to whatever name is appropriate.

```sh
micromamba create -n beacon python=3.12.2
```

Now activate the new environment

```sh
micromamba activate beacon
```

Install flask and all necessary packages (`flask`, `flask-sqlalchemy` and `flask-migrate`):

```sh
micromamba install flask flask-sqlalchemy  conda-forge::flask-migrate
```

## Create DB and Run

```sh
flask db init
flask db migrate -m 'Initial Table Creation'
flask upgrade
```

That should create an database in `db/app.db`.

To start the development webserver run:

```sh
flash run
```

If you want to run this in production make sure you configure appropriate the `config.py` under the `ProductionConfig` and set the environment

## Deploy with `gunicorn` and `nginx`

To deploy on the flask app through `nginx` you need the following steps:

### User to run the flask app

You can either create a new user or use the `git` user. In this example `git` user is used. (If you create a new user, you need to modify the `geminoa-flask.service` in the subsequent step).

### Create a python environment

For the `git` user, install the python environment. For `micromamba` you can use the `environment.yml` with the following command:

```sh
micromamba env create -f environment.yml
```

You also need to install `gunicorn` with `envetlet`:

```sh
micromamba activate geminoa-flask
pip3 install gunicorn[eventlet]
```

### Configure `gunicorn`

Create a `secrets-and-variables` file following the `secrets-and-variables.template` and edit the environmental required by the `systemd` service to run `gunicorn` behind `nginx`. In particular you need to edit the username and password for the PostgreSQL URI. The variable `SCRIPT_NAME` is needed if you are deploying the flask under a prefix URI (in this example `/backend`). If you are deploying on root URI (`/`) you can comment this line out. Make sure that the folder refences in the `GUNICORN_ERROR_LOGFILE` and `GUNICORN_ACCESS_LOGFILE` exists and has permissions `git:www-data`.

### Install the service file in `systemd`

Inspect the `geminoa-flask.service` and you need to make sure that the paths on `WorkingDirectory`, `EnvironmentFile` and the path to the `gunicorn` binary binary in `ExecStart` are correct. Then move the `geminoa-flask.service` to `/etc/systemd/system` and run:

```sh
systemctl daemon-reload
systemctl start geminoa-flask.service
systemctl enable geminoa-flask.service
systemctl status geminoa-flask.service
```

### Configure `nginx`

Use a standard server block with the location:

```config
location /backend {
    include proxy_params;
    proxy_set_header SCRIPT_NAME "/backend";
    proxy_pass http://unix:/var/www/geminoa/geminoa/flask/geminoa-flask.sock;
}
```

In the above adjust the path to the unix socket which is defined in the in the `geminoa-flask.service` and in particular in the `WorkingDirectory` variable. Restart `nginx`
