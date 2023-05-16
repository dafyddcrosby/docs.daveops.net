# Configuration Management


# Ansible


## Run a playbook

```shell
ansible-playbook -i ./inventory.yml playbook.yml
```


# Chef


## Handle EC2 instance

ec2 plugin installed with

```shell
knife ec2 server create "role[ubuntu]" -I ami_id -f instance_type -S knife -i ~/.ssh/knife.pem --ssh-user ubuntu --region eu-west-1 -Z eu-west-1a
```


## Install chef on RHEL 6 using gems

Use the omnibus installer if you can!

```shell
sudo rpm -Uvh <http://rbel.frameos.org/rbel6>
yum install ruby ruby-devel ruby-ri ruby-rdoc ruby-shadow gcc gcc-c++ automake autoconf make curl dmidecode
gem install chef --no-ri --no-rdoc
```


## Using chef-solo


### /etc/chef/solo.rb

```ruby
json_attribs "/etc/chef/node.json"
```


### /etc/chef/node.json

```json
{
 "resolver": {
       "nameservers": [ "10.0.0.1" ],
       "search": "int.example.com"
  },
  "run_list": [ "recipe[resolver]" ]
}
```


## knife search

```shell
knife search -a ATTR
```


## common node attributes

| description         | attribute                    |
|------------------- |---------------------------- |
| version of chef     | `chef_packages.chef.version` |
| nodes's environment | `chef.environment`           |


## Compile time notes

Use `lazy` so that the code block isn't evaluated until execution phase.


# Inspec

<https://www.inspec.io/>


# Chef shell

```ruby
# list resources
help resource
```


## Debug attributes

```ruby
pp node.debug_value('system', 'repo')
```


## Get the resources used

```ruby
# in chef-shell
recipe_mode
resources
```


# ChefSpec

- [Chef docs](https://docs.chef.io/workstation/chefspec/)
- [Github repo](https://github.com/chefspec/chefspec)


# Foodcritic

Has been replaced by cookstyle

```shell
# Run rules that match the tags
foodcritic -t annoyances,deprecated,correctness
```


# Chef Handlers


## Handler types


### exception

Loaded when run fails

exception handler runs when the `failed?` property for the run<sub>status</sub> object returns true.


### report

Reports details of run success

report handler runs when the `success?` property for the run<sub>status</sub> object returns true.


### start

Starts at chef client run


## Resources

<https://docs.chef.io/handlers.html>


# Knife


## Bootstrap a node

```shell
knife bootstrap FQDN_OR_IP -E ENVIRONMENT -N NAME -x USER -r RUN_LIST [ --sudo | -G GATEWAY ]
```


## Return chef versions

```shell
knife search node "name:*" -a chef_packages.chef.version
```


## Remove recipe from all nodes

```shell
knife exec -E 'nodes.transform("chef_environment:dev") {|n| puts n.run_list.remove("recipe[chef-client::upgrade]"); n.save }'
```


## Find non 64-bit nodes

```shell
knife search node "(NOT kernel_machine:x86_64)"
```


## Generate new keypair for client

```shell
knife client reregister CLIENT
```


# provisioning


## CLI

```shell
chef provision --no-policy
# debug mode
chef provision --no-policy -D
```


# Berkshelf


## CLI

```shell
# Install cookbooks
berks install
```


## Berksfile

```ruby
## In case you're developing on a bunch of cookbooks
source chef_repo: ".."

# You'll need this if you're downloading upstream cookbooks
source "https://supermarket.chef.io"

metadata

# Use specific directory in git repo
cookbook "rightscale", git: "https://github.com/rightscale/rightscale_cookbooks.git", rel: "cookbooks/rightscale"
```


# Test Kitchen


## Setting RAM on Vagrant

```yaml
driver:
  name: vagrant
  customize:
    memory: 2048
```


## Setting environment in chef-zero

```yaml
driver:
  name: vagrant

provisioner:
  name: chef_zero
  environments_path: path/to/environments
  client_rb:
    environment: production
```


## kitchen-dokken

Pre-installed with ChefDK

```yaml
---
driver:
  name: dokken
  chef_version: latest

transport:
  name: dokken

provisioner:
  name: dokken

verifier:
  name: inspec

platforms:
- name: centos-7
  driver:
    image: dokken/centos-7

suites:
  - name: default
    run_list:
    - recipe[hello_dokken::default]
```


## Using chef-vault

```yaml
suites:
  - name: default
      data_bags_path: 'test/data_bags'
      attributes:
      chef-vault:
        databags_fallback: true
      dev_mode: true # For very old chef-vault cookbooks
    run_list:
    - recipe[chef-vault]
```


## Links

- [Test Kitchen website](https://kitchen.ci)
- <https://github.com/someara/kitchen-dokken>


# Server


## Users

```shell
# Create a user
chef-server-ctl user-create USER_NAME FIRST_NAME [MIDDLE_NAME] LAST_NAME EMAIL PASSWORD (options)
# Edit a user
chef-server-ctl user-edit USER_NAME
# Delete a user
chef-server-ctl user-delete USER_NAME
```


## Organizations

```shell
# List orgs
chef-server-ctl org-list
# Create an org
chef-server-ctl org-create ORG_NAME ORG_FULL_NAME
# Add a user to an org
chef-server-ctl org-user-add ORG_NAME USER_NAME [--admin]
```


## Groups

[knife acl plugin](https://github.com/chef/knife-acl)

```shell
knife group create GROUP
knife group add MEMBERTYPE MEMBER GROUP
```


## Links

- [chef-server-ctl](https://docs.chef.io/ctl_chef_server.html)
- [Air-gapped servers](https://docs.chef.io/install_chef_air_gap.html)


# Chef-Vault


## Knife

```shell
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

- [chef vault gem](https://github.com/chef/chef-vault)
- [chef vault cookbook](https://github.com/chef-cookbooks/chef-vault)
- <http://www.pburkholder.com/blog/2015/12/04/why-chef-vault-and-autoscaling-dont-mix/>
- <http://engineering.ooyala.com/blog/keeping-secrets-chef>


# Concepts - Idempotency

Something that should always have the same result, no matter how many times it runs.

```shell
# /tmp/thing should exist no matter how many times you run this:
mkdir -p /tmp/thing
```