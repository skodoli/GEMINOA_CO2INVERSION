#!/usr/bin/env python3
import os
from textwrap import dedent


def wait_for_enter():
    input("\nPress enter to continue...")
    os.system("clear")


def add_wifi_network(context):
    msg = """
    Add Wifi Network
    ================

    Add wifi network for the 4G router (if using one), or known wifi address.
    Optionally, add your phone hotspot for easier debugging.

    Run:
        sudo nano /etc/wpa_supplicant/wpa_supplicant.conf

    And add the following network config:

        network={{
            ssid=\"StrathBeacon\"
            scan_ssid=1
            psk=\"gondor_strath\"
            priority=8
        }}"""

    print(dedent(msg).strip())
    wait_for_enter()


def update_network_interface(context):
    msg = """
    Update network interface
    ========================

    Run:
        sudo nano /etc/network/interfaces

    In the above file, remove the '#' from the start of the following lines:
        #allow-hotplug wlan0
        #iface wlan0 inet dhcp
        #    wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf
    """
    print(dedent(msg).strip())
    wait_for_enter()


def connect_to_beacon(context):
    print("Connect to the beacon through putty, ssh, or a wired keyboard and monitor.")
    wait_for_enter()


def refresh_wifi_module(context):
    msg = """
    Ensure the Pi detects the network config
    ========================================

    Run the following commands:
        sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
        sudo wpa_cli -i wlan0 reconfigure

    You MIGHT get kicked off wifi and need to reconnect.
    """
    print(dedent(msg).strip())
    wait_for_enter()


def update_data_sync_frequency(context):
    msg = """
    Reduce data sync frequency
    ==========================

    Edit the crontab. Run:
        crontab -e

    Change each rsync line to look like:
        15 */4 * * * rsync ...long command for data...
        30 */4 * * * rsync ...long command for logs...

    """
    print(dedent(msg).strip())
    wait_for_enter()


def update_vpn_connection_schedule(context):
    msg = """
    Reduce frequency of VPN checks
    ==============================

    Edit the root crontab. Run:
        sudo crontab -e

    And change the line with 'vpn_check' to:
        1 */4 * * * /usr/local/bin/vpn_check
    """
    print(dedent(msg).strip())
    wait_for_enter()


def update_hardware_clock(context):
    msg = """
    Update hardware clocks
    ======================

    Connect to the pi through something with an outbound internet connection.

    Run the commands:
        sudo date -s \"$(curl -s --head https://google.com | grep -i ^Date: | sed -e 's/[Dd]ate: //g')\"
        sudo hwclock -w
        timedatectl
    """
    print(dedent(msg).strip())
    wait_for_enter()


def reboot_pi(context):
    msg = """
    Reboot the Pi
    =============

    Run:
        sudo reboot now
    """
    print(dedent(msg).strip())
    wait_for_enter()


if __name__ == "__main__":
    context = {"password": "CO2Network2012"}
    steps = [
        connect_to_beacon,
        add_wifi_network,
        update_network_interface,
        refresh_wifi_module,
        update_data_sync_frequency,
        update_vpn_connection_schedule,
        update_hardware_clock,
        reboot_pi,
    ]

    os.system("clear")
    for step in steps:
        step(context)
    print("Done.")
