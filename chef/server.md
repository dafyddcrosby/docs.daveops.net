---
title: server
tags: ["Chef"]
---

## Users

```bash
# Create a user
chef-server-ctl user-create USER_NAME FIRST_NAME [MIDDLE_NAME] LAST_NAME EMAIL PASSWORD (options)
# Edit a user
chef-server-ctl user-edit USER_NAME
# Delete a user
chef-server-ctl user-delete USER_NAME
```

## Organizations

```bash
# List orgs
chef-server-ctl org-list
# Create an org
chef-server-ctl org-create ORG_NAME ORG_FULL_NAME
# Add a user to an org
chef-server-ctl org-user-add ORG_NAME USER_NAME [--admin]
```

## Groups

[knife acl plugin](https://github.com/chef/knife-acl)

```bash
knife group create GROUP
knife group add MEMBERTYPE MEMBER GROUP
```

## Links

* [chef-server-ctl](https://docs.chef.io/ctl_chef_server.html)
* [Air-gapped servers](https://docs.chef.io/install_chef_air_gap.html)
