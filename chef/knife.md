---
title: knife
---

## Bootstrap a node
```bash
knife bootstrap FQDN_OR_IP -E ENVIRONMENT -N NAME -x USER -r RUN_LIST [ --sudo | -G GATEWAY ]
```

## Return chef versions
```bash
knife search node "name:*" -a chef_packages.chef.version
```

## Remove recipe from all nodes
```bash
knife exec -E 'nodes.transform("chef_environment:dev") {|n| puts n.run_list.remove("recipe[chef-client::upgrade]"); n.save }'
```

## Find non 64-bit nodes
```bash
knife search node "(NOT kernel_machine:x86_64)"
```

## Generate new keypair for client
```bash
knife client reregister CLIENT
```
