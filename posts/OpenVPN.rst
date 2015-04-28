OpenVPN
-------
:date:

Setting up an OpenVPN server
==============================
::

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

Using a static key
==================
::

 # generate static key
 openvpn --genkey --secret static.key

In configuration files:

::

 secret static.key
 # or
 <tls-auth>
 Key contents
 </tls-auth>
