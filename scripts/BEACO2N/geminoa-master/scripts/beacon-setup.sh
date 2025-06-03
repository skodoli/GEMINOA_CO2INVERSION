#!/usr/bin/env bash
set -euo pipefail

wait_for_response() {
    local var_to_ignore
    echo -n "Press enter to continue..."
    read var_to_ignore
}

warn_about_sudo(){
    local response
    echo "This script will ask for your password to edit system configuration"
    read -n 1 -p "Continue? [Yn]   " response
    case $response in
        Y|y) return ;;
        *)
            echo "EXITING"
            exit 1 ;;
    esac
    clear
}

connect_to_beacon() {
    echo "Connect to the beacon through putty, ssh, or a wired keyboard and monitor."
    wait_for_response
    clear
}

add_wifi_id() {
    echo "Add a wifi network to /etc/wpa_supplicant/wpa_supplicant.conf"
    echo "and ensure at the top, we have country=GB"
    echo "Optionally, add your personal hotspot, for easier debugging"
    if [[ -n $(grep "StrathBeacon" /etc/wpa_supplicant/wpa_supplicant.conf) ]]; then
        echo "!!! Already has StrathBeacon wifi network details."
    else
        cat << EOF | sudo tee -a /etc/wpa_supplicant/wpa_supplicant.conf
network={
    ssid="StrathBeacon"
    scan_ssid=1
    psk="gondor_strath"
    priority=8
}
EOF
    fi
    wait_for_response
    clear
}

update_network_interface() {
    echo "Edit /etc/network/interfaces and remove leading '#' from the following lines:"
    echo
    echo "    #allow-hotplug wlan0"
    echo "    #iface wlan0 inet dhcp"
    echo "    #    wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf"
    echo
    echo "WILL LAUNCH EDITOR"
    wait_for_response
    sudo $EDITOR /etc/network/interfaces
    echo "NETWORK INTERFACES EDITED"
    wait_for_response
    clear
}

refresh_wifi_module() {
    echo "Ensure wifi is using updated config"
    sudo wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
    sudo wpa_cli -i wlan0 reconfigure
    wait_for_response
    clear
}

update_data_sync_schedule() {
    echo "Edit user crontab to change sync schedule to look like..."
    echo
    echo "    15 */4 * * * rsync ...long command for data..."
    echo "    30 */4 * * * rsync ...long command for logs..."
    echo
    echo "Default is something like '1 * * * * rsync ...'"
    echo "WILL NOW OPEN EDITOR"
    wait_for_response
    crontab -e
    wait_for_response
    clear
}

update_vpn_connection_schedule() {
    echo "Edit the root crontab to reduce vpn connection checks"
    echo
    echo "Change the vpn_check line to:"
    echo
    echo "    1 */4 * * * /usr/local/bin/vpn_check"
    echo
    echo "Default is something like '* * * * * ...vpn_check...'"
    echo
    echo "WILL NOW OPEN EDITOR"
    wait_for_response
    sudo crontab -e
    wait_for_response
    clear
}

update_clocks() {
    echo "(You need an internet connection for this)"
    echo "We will read google's header for a date to set the hwclock and sys clock."
    echo
    wait_for_response
    sudo date -s "$(curl -s --head https://google.com | grep -i ^Date: | sed -e 's/[Dd]ate: //g')"
    sudo hwclock -w
    timedatectl
    wait_for_response
    clear
}

reboot_pi() {
    echo "WILL NOW REBOOT"
    wait_for_response
    sudo reboot now
}

connect_to_beacon
warn_about_sudo
add_wifi_id
update_network_interface
refresh_wifi_module
update_data_sync_schedule
update_vpn_connection_schedule
update_clocks
reboot_pi
