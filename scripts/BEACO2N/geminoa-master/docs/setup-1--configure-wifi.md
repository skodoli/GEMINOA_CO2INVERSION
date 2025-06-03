# Setup -- Configure Wifi

For a new beacon, need to connect *physically* (i.e. keyboard and monitor) *(cd: or maybe with ethernet and ssh?)* and add some wifi details.

## Connect to the beacon

**If connecting physically:**

-   Login
    -   username: `beacon`
    -   password: `CO2Network2012`

**If connecting via ssh:**

-   Find the IP (in your router menu or something like `ipconfig/ifconfig`)
-   ssh to the device and login
    -   username: `beacon`
    -   password: `CO2Network2012`

## Edit the file `/etc/wpa_supplicant/wpa_supplicant.conf`

This file is where wifi passwords etc are stored.

-   edit the file by running a command something like `sudo nano /etc/wpa_supplicant/wpa_supplicant.conf`
    -   ...or whatever other editing comand you're familiar with
-   default password for `sudo` is `CO2Network2012`

First, ensure the top of the file has `country=GB`. Change it if it doesn't (typically starts off as US).

Next, add a wifi network that matches what you'll use. Our default is `StrathBeacon`, for some external 4G SIM-powered routers. Also add your own phone's hotspot for debugging, if you want, for convenience. Here, `davison` is my phone's hotspot name.

    network={
            ssid="StrathBeacon"
            scan_ssid=1
            psk="gondor_strath"
            priority=8
    }

    network={
            ssid="davison"
            scan_ssid=1
            psk="asdfghjkl"
            priority=8
    }

### Update the network interface

**Update network interface** `/etc/network/interfaces` by deleting the `#` (uncommenting) from the start of the following lines:

    allow-hotplug wlan0
    iface wlan0 inet dhcp
        wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf

### 1.3 Refresh the wifi module

    # maybe...although not sure how to do this over wifi
    # sudo pkill wpa_supplicant

    sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
    sudo wpa_cli -i wlan0 reconfigure

You *might* get kicked off wifi and need to reconnect.
