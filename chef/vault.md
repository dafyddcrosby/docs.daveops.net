---
title: Chef/vault
---

## Knife

```bash
# Create a vault
knife vault create passwords root '{"username": "root", "password": "mypassword"}' -S "role:webserver"

# Re-encrypt the vault with a fresh search of nodes
knife vault refresh passwords root

# Update the search for hosts on a vault
knife vault update passwords root -S "role:webserver"

# Create a vault from a file
knife vault create certs example.com --file example.crt

# Get the search query
knife data bag show VAULT ITEM_keys -Fjson | jq .search_query
```

## Code

### chef-vault cookbook

```ruby
include_recipe "chef-vault"
vault = chef_vault_item(DATABAG, ITEM)
```

### chef-vault gem

```ruby
chef_gem 'chef-vault' do
  compile_time true if respond_to?(:compile_time)
end

require 'chef-vault'

item = ChefVault::Item.load("passwords", "root")
item["password"]
```

## Links

* [chef vault gem](https://github.com/chef/chef-vault)
* [chef vault cookbook](https://github.com/chef-cookbooks/chef-vault)

* <http://www.pburkholder.com/blog/2015/12/04/why-chef-vault-and-autoscaling-dont-mix/>
* <http://engineering.ooyala.com/blog/keeping-secrets-chef>
