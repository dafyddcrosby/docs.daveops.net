# Android
Keystore
--------
	# Create a keystore
	keytool -genkey -v -keystore /path/to/example-key.keystore -keyalg RSA -keysize 2048 -alias alias_name -validity 10000
	# Get expiration dates in the store
	keytool -list -v -keystore keystore.jks


Auto-sign release
-----------------
Add to ant.properties

	key.store=/path/to/example-key.keystore
	key.alias=alias_name

