OpenVPN
-------


Setting up an OpenVPN server
==============================
{{{
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
}}}

