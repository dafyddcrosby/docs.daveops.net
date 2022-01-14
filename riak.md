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
