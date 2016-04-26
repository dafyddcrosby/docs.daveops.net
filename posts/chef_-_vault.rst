Chef - Vault
============
:date: 2016-4-26

Knife
-----
::

  # Create a vault
  knife vault create passwords root '{"username": "root", "password": "mypassword"}' -S "role:webserver"
  # Re-encrypt the vault with a fresh search of nodes
  knife vault refresh passwords root
  # Update the search for hosts on a vault
  knife vault update passwords root -S "role:webserver"

