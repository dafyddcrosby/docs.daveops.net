# BlackBerry

## Compile via command-line

```bash
source bbndk-env.sh
export PROJECT_DIR=<project directory>
qcc main.c -o main
```

## View logs via SSH

```bash
blackberry-connect 169.254.0.1 -password <pass>
ssh devuser@169.254.0.1
```

## Create developer certificate

 blackberry-keytool -genkeypair -keystore ~/.rim/author.p12 -storepass <pw> -author <author>

## Create debug token

 blackberry-debugtokenrequest -bbidtoken ~/.rim/bbidtoken.csk -storepass <store_pw> -deviceid <device_id> ~/.rim/debug_token.bar

## Deploy debug token

 blackberry-deploy -installDebugToken ~/.rim/debug_token.bar -device <IP address> -password <device password>

## Build a native package

 # build with a debug token
 blackberry-nativepackager -package <package>.bar bar-descriptor.xml -devMode -debugToken ~/.rim/debug_token.bar

## Deploy native app

 blackberry-deploy -installApp 169.254.0.1 -password <device_pw> <package>.bar

## Reference documentation

* [Native API Reference](https://developer.blackberry.com/playbook/native/reference/)
