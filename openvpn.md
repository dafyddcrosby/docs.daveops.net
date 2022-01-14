# OpenVPN

## Setting up an OpenVPN server

```bash
# Set up a cert. authority
cd /etc/openvpn/easy-rsa/
# Edit the vars file
. ./vars
./clean-all
./build-ca
# Create server certs
./build-key-server server
# Create client certs
./build-key client1
# Build Diffie Hellman parameters
./build-dh
```

## Hardening

* Explicitly set the server `cipher`, `tls-cipher` and `auth`
* Use at least 2048-bit RSA keys
* Use `tls-auth` to mitigate DDoS
* Keep CA PKI secure. If the CA key is compromised, you'll need to reissue
* Generate private keys on the target system
* Use strong key passphrases
* Avoid sharing keys across targets
* Generate a CRL at the creation of a VPN
* Use Diffie-Hellman parameters of 2048-bit+
* Set the `script-security` level to what is appropriate
* If you've got servers+clients greater than OpenVPN 2.3.2, set `tls-version-min` to 1.2

## CLI misc

```bash
# See list of supported ciphers
openvpn --show-ciphers
# See list of supported HMACs
openvpn --show-digests
# See list of supported TLS cipher-suites
openvpn --show-tls
```

## Using a static key

```bash
# generate static key
openvpn --genkey --secret static.key
```

In configuration files:

```
secret static.key
# or
<tls-auth>
Key contents
</tls-auth>
```
