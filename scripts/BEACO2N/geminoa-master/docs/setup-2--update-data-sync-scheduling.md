# Setup -- Update (reduce) data sync scheduling

The default settings are fine for an unlimited 'home' setup, but on a pay-as-you-go or datacap'd SIM router, it can be expensive. By default, it spits out \~500kb every minute.

In order to change this, you need to update both the user *and* the root `crontab`.

Run `crontab -e` to edit the user crontab, and change each of the lines to look something like:

    15 */4 * * * rsync ...long command...

    30 */4 * * * rsync ...long command...

e.g. 15 minutes past *every 4th hour*. By default, it's something like `*/1 * * * *` which is *every minute of every hour*.

Run `sudo crontab -e` to edit the admin crontab, and change the vpn line to:

    1 */4 * * * /usr/local/bin/vpn_check

e.g. only check the VPN is up just before we transmit data.

***N.B.*** You shouldn't *need* to reboot the pi, but it's perhaps best to, to make sure the crontab changes are picked up.
