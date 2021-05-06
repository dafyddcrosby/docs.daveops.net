---
title: memcached
---

## Get a list of keys

Note - this is an undocumented interface in the C memcached server

```
# get slab info
stats items
# get dump of slab
stats cachedump SLAB LIMIT
```

## Using ASCII protocol through telnet

```bash
telnet host 11211
```

command                                                                                                                                                                    | description
---                                                                                                                                                                        | ---
stats                                                                                                                                                                      | gets buncha stats
set <key> <flags> <exptime> <bytes> [noreply]\r                                                                                                                            | set a value
get <key> - returns: VALUE <key> <flags> <bytes> [<cas unique>]\r<data block>\r                                                                                            |  |


\*:<bytes> is the number of bytes in the data block to follow, *not* including the delimiting \r <bytes> may be zero (in which case it's followed by an empty data block). |  |

## Using Dalli

```ruby
require 'dalli'
dc = Dalli::Client.new('localhost:11211')
dc.set('foo', 'bar')
dc.get('foo')
```

## Resources

* [memcached site](http://memcached.org/)
* [libmemcached site](http://libmemcached.org/)
* [memdump](http://docs.libmemcached.org/bin/memdump.html)
* [Docker image](https://hub.docker.com/_/memcached)
* [Ruby client](https://github.com/petergoldstein/dalli)

* [LRU tuning](https://memcached.org/blog/modern-lru/)
