---
title: Berkshelf
tags: ["Chef"]
---

## CLI

```bash
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
