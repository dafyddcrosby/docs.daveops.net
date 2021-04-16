---
title: Chef/Test Kitchen
tags: ["Chef"]
---

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

* [Test Kitchen website](https://kitchen.ci)
* <https://github.com/someara/kitchen-dokken>

