# Secrets Management
# HashiCorp vault

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
# Get path help
vault path-help -h
```

## Environment Variables

| VAULT_ADDR | vault address |

## Server

```bash
# start a dev server
vault server -dev

# initialize a repo, generate a root token
# NOTE - you should *only* use a root token for initial setup/emergencies
vault operator init -recovery-shares=1 -recovery-threshold=1
```

## Links
* https://learn.hashicorp.com/vault/day-one/production-hardening
* https://www.hashicorp.com/blog/using-hashicorps-vault-with-chef
* [Docker image](https://hub.docker.com/_/vault)


# passwordstore

[core](https://www.passwordstore.org/)
[gopass](https://www.gopass.pw/)
