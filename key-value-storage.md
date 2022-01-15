# Key-Value storage
# Riak

## Get stats

	 curl -H "Accept: text/plain" http://127.0.0.1:8098/stats

### Riak pool maintenance
	# Add the node to the cluster
	riak-admin cluster join ${NODE}
	
	# Create a plan to see how the new ring will be laid out
	riak-admin cluster plan
	
	# Commit the plan
	riak-admin cluster commit
	
	# Remove a node from the cluster
	riak-admin cluster leave ${NODE}

### Restarting a node
	riak-admin transfers # ensure cluster is in good state
	riak-admin member-status # get status of all cluster members

## Ports

<http://docs.basho.com/riak/kv/2.2.3/using/security/>

desc                    | port
---                     | ---
epmd listener           | TCP:4369
handoff_port listener   | TCP:8099
internode communication | TCP:6000-7999
protocol buffers        | TCP:8087
HTTP                    | TCP:8098

## Links

* [Docker image](https://hub.docker.com/r/basho/riak-kv)
* [Ruby client](https://github.com/basho/riak-ruby-client)
# memcached

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


## moxi

* [Configuration](https://github.com/steveyen/moxi/blob/master/doc/moxi/configuration.org)


# Redis

## Links
* [Docker image](https://hub.docker.com/_/redis)
* https://redis.io/
* [Ruby client](https://github.com/redis/redis-rb)
# etcd

https://etcd.io/
