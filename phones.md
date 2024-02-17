# Phones


# Systems

<https://en.wikipedia.org/wiki/Signalling_System_No._7>


# Numbers

| 00       | International operator |
| 555-1212 | directory assistance   |


# BlackBerry


## Compile via command-line

```shell
source bbndk-env.sh
export PROJECT_DIR=<project directory>
qcc main.c -o main
```


## View logs via SSH

```shell
blackberry-connect 169.254.0.1 -password <pass>
ssh devuser@169.254.0.1
```


## Create developer certificate

```shell
blackberry-keytool -genkeypair -keystore ~/.rim/author.p12 -storepass $PW -author $AUTHOR
```


## Create debug token

```shell
blackberry-debugtokenrequest -bbidtoken ~/.rim/bbidtoken.csk -storepass $PW -deviceid $DEVICEID ~/.rim/debug_token.bar
```


## Deploy debug token

```shell
blackberry-deploy -installDebugToken ~/.rim/debug_token.bar -device $IP -password $DEVICEPW
```


## Build a native package

```shell
# build with a debug token
blackberry-nativepackager -package $PKG.bar bar-descriptor.xml -devMode -debugToken ~/.rim/debug_token.bar
```


## Deploy native app

```shell
blackberry-deploy -installApp 169.254.0.1 -password $PW $PKG.bar
```


## Reference documentation

- [Native API Reference](https://developer.blackberry.com/playbook/native/reference/)
