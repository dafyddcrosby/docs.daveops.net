---
title: vault
---

## Client

```bash
# get status of the vault setup
vault status
# get list of secrets engines
vault secrets list
# add a secret
vault kv put foo/bar hello=world
# get a secret
vault kv get foo/bar
# specific field
vault kv get -field=hello foo/bar
```

## Environment Variables

| VAULT_ADDR | vault address |

## Server

```bash
# start a dev server
vault server -dev
```

## Links
* https://learn.hashicorp.com/vault/day-one/production-hardening
* https://www.hashicorp.com/blog/using-hashicorps-vault-with-chef
