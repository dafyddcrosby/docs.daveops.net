# Key-Value storage


# Riak


## Get stats

```
curl -H "Accept: text/plain" http://127.0.0.1:8098/stats
```


### Riak pool maintenance

```
# Add the node to the cluster
riak-admin cluster join ${NODE}

# Create a plan to see how the new ring will be laid out
riak-admin cluster plan

# Commit the plan
riak-admin cluster commit

# Remove a node from the cluster
riak-admin cluster leave ${NODE}
```


### Restarting a node

```
riak-admin transfers # ensure cluster is in good state
riak-admin member-status # get status of all cluster members
```


## Ports

<http://docs.basho.com/riak/kv/2.2.3/using/security/>

| desc                            | port          |
|------------------------------- |------------- |
| epmd listener                   | TCP:4369      |
| handoff<sub>port</sub> listener | TCP:8099      |
| internode communication         | TCP:6000-7999 |
| protocol buffers                | TCP:8087      |
| HTTP                            | TCP:8098      |


## Links

- [Docker image](https://hub.docker.com/r/basho/riak-kv)
- [Ruby client](https://github.com/basho/riak-ruby-client)


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

```shell
telnet host 11211
```


## Using Dalli

```ruby
require 'dalli'
dc = Dalli::Client.new('localhost:11211')
dc.set('foo', 'bar')
dc.get('foo')
```


## Resources

- [memcached site](http://memcached.org/)

- [libmemcached site](http://libmemcached.org/)

- [memdump](http://docs.libmemcached.org/bin/memdump.html)

- [Docker image](https://hub.docker.com/_/memcached)

- [Ruby client](https://github.com/petergoldstein/dalli)

- [LRU tuning](https://memcached.org/blog/modern-lru/)


## moxi

- [Configuration](https://github.com/steveyen/moxi/blob/master/doc/moxi/configuration.org)


# Redis

- [Docker image](https://hub.docker.com/_/redis)
- <https://redis.io/>
- [Ruby client](https://github.com/redis/redis-rb)


# etcd

<https://etcd.io/>