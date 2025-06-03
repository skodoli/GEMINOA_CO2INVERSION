# Setup -- Update system and hardware clock

*Requires internet connection.* If you are using a dumb router just to connect to the pi (e.g. in the TIC lab, can be an issue adding routers to the network so you won't have outbound), you are probably best using either your own phone or the StrathBeacon 4G routers.

1.  Connect to strathbeacon wifi from laptop (or connect to your phone hotspot)
    a.  SSID `StrathBeacon`
    b.  PW `gondor_strath`
2.  Go to admin panel (`192.168.199.1` with uname `admin` pw `admin`)
    a.  ...or look at your phone settings page to see connected device's IP
3.  Find IP of connect device
4.  Use putty to connect to device
    a.  username `beacon`
    b.  password `CO2Network2012`

As root user, run the command:

    sudo date -s "$(curl -s --head https://google.com | grep -i ^Date: | sed -e 's/[Dd]ate: //g')"
    sudo hwclock -w
    timedatectl

(This just reads google's homepage, and gets the date from the header, in a format something like `Tue, 02 Nov 2021 12:39:02 GMT`)
