# Electronic hardware

- [Navy Electricity and Electronics Training Series](https://www.hnsa.org/resources/manuals-documents/2575-2/)
- <https://hackaday.com/>


## FPGA

- <https://opencores.org>
- <http://www.fpgaarcade.com/>


## Parts manufacturers/distributors/sellers

- <https://www.mouser.com/>
- <https://www.digikey.ca>
- <https://www.jameco.com>
- <https://solarbotics.com/>


## Documentation

- <http://www.alldatasheet.com/>


## Snippets

- data sheets can tell you what's there, but omissions in data (eg [ESD](https://warmcat.com/2016/11/21/let's-play-what's-my-esd-rating.html)) can bite you


# Fuses

Slow blow fuses:

- L - Low Breaking Capacity (ie glass)
- H - High Breaking Capacity (ie ceramic)


# theory


## Voltage

- symbol: V (sometimes E)
- measured in volts
- voltage is *between 2 points*


## Current

- symbol: I
- measured in amps
- current is *through a circuit*


# Arduino

<http://ardx.org/src/code/>


# Raspberry Pi


## Installing Fedora

- <https://arm.fedoraproject.org/>
- <https://docs.fedoraproject.org/en-US/quick-docs/raspberry-pi/>

```shell
dnf install -y fedora-arm-installer
fedora-arm-image-installer --image=</path/to/fedora_image> --target=<RPi_Version> --media=/dev/<sd_card_device> --resizefs
```


## Pi 3


### UART

- 115200 baud rate
- 3.3V logic

Because of a [firmware issue](https://www.raspberrypi.org/forums/viewtopic.php?f=28&t=141195), need to add this to `/boot/config.txt`:

```
enable_uart=1
```

[See also the GH issue](https://github.com/raspberrypi/firmware/issues/553#issuecomment-199486644)


# Oracle Integrated LOM


## Turn on indicator light


### 3.0

```
show /SYS/LOCATE
set /SYS/LOCATE value=Fast_Blink
```


## Turn server off

```
stop /SYS
```


# USB


## Cable

- Red +5V power
- Black Ground
- White Data (-)
- Green Data (+)


# RAID levels

| Level | Data distribution                                                     | Performance                                                                      | Space availability                                                                                                                      | Redundancy                                        |
| 0     | Data is striped over multiple disks.                                  | Higher read/write perf, since it's split over disks                              | All disks are available as one pool                                                                                                     | None - loss of one disk is catastrophic           |
| 1     | Available disk is effectively halved.                                 | Data is mirrored over multiple disks                                             | Read performance increased since multiple blocks can be accessed at the same time. Write perf declines since data must be written twice | Loss of 1 disk is non-catastrophic                |
| 4     | Data striped over multiple disks. Parity information on separate disk | Read operations benefit by striping.  Writes are bottlenecked on the parity disk | More efficient use of data redundancy with parity disk                                                                                  | If parity disk is lost, data redundancy is lost   |
| 5     | Data and parity striped over multiple disks                           | Read operations benefit by striping                                              |                                                                                                                                         | Can only handle one disk failure before data loss |
| 6     | Data and dual parity blocks striped over multiple disks               | Read operations benefit by striping                                              |                                                                                                                                         | Can handle two disk failures before data loss     |
| 10    | Hybridizing of 1 + 0, striping over mirrored sets                     | Read operations benefit by striping                                              |                                                                                                                                         |                                                   |
