---
title: gems
---

## Build a gem

```bash
gem build name.gemspec
```

## Uploading

```bash
# push gem to rubygems.org or other host
gem push name-0.0.1.gem [--host HOST]
```

## Sources

```bash
# add a source
gem source -a SOURCE
# remove a source
gem source -r SOURCE
# update source cache
gem source -u
```

## Ownership

```bash
gem owner GEM --add EMAIL
gem owner GEM --remove EMAIL
```

## Environment variables

| var      | description              |
|----------|--------------------------|
| GEM_PATH | where gems can be loaded |
| GEM_HOME | where gems are installed |


## Links

* [Gem packaging best practices](http://weblog.rubyonrails.org/2009/9/1/gem-packaging-best-practices/)
