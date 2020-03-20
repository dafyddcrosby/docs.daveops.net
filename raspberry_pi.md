---
title: Raspberry Pi
---

## Installing Fedora

* https://arm.fedoraproject.org/
* https://docs.fedoraproject.org/en-US/quick-docs/raspberry-pi/

```bash
dnf install -y fedora-arm-installer
fedora-arm-image-installer --image=</path/to/fedora_image> --target=<RPi_Version> --media=/dev/<sd_card_device> --resizefs
```

## Pi 3

### UART

* 115200 baud rate
* 3.3V logic

Because of a [firmware
issue](https://www.raspberrypi.org/forums/viewtopic.php?f=28&t=141195), need to
add this to `/boot/config.txt`:
```
enable_uart=1
```
[See also the GH issue](https://github.com/raspberrypi/firmware/issues/553#issuecomment-199486644)
