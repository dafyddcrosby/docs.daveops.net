---
title: RabbitMQ
tags: ["Erlang"]
---

## add/restart/remove cluster node

```bash
# Add a node
rabbitmqctl stop_app
rabbitmqctl join_cluster rabbit@rabbit2
rabbitmqctl start_app

# Restart a node
rabbitmqctl stop
rabbitmq-server -detached

# Remove a node (locally)
rabbitmqctl stop_app
# Remove a node (remotely)
rabbitmqctl forget_cluster_node rabbit@rabbit1

# Get cluster status
rabbitmqctl cluster_status

# Rotate logs
rabbitmqctl rotate_logs <suffix>
```

## Ports

* 4369 - EPMD
* 5672 - AMQP connections
* 15672 - mgmt interface

## Re-syncing mirrors in HA mode

```bash
rabbitmqctl list_queues name slave_pids synchronised_slave_pids
# to see where it's not being synced:
rabbitmqctl list queues name synchronised_slave_pids | grep -v node_name > output

rabbitmqctl sync_queue name
# can do a sad little bash loop after cleaning the output file to just queue names
for i in `cat output` ; do sudo rabbitmqctl sync_queue $i ; done
```

## General tuning tips

* Set ulimit -n and fs.file-max to 500,000
* Set low keepalives
  * net.ipv4.tcp_keepalive_time = 6
  * net.ipv4.tcp_keepalive_intvl = 3
  * net.ipv4.tcp_keepalive_probes = 3
* For higher throughput, use larger TCP buffers
  * net.core.rmem_max = 16777216
  * net.core.wmem_max = 16777216
* For more concurrent connections, smaller TCP buffers
  * low tcp_fin_timeout
  * tcp_tw_reuse = 2
* Reduce per connection RAM use (drops throughput)
  * rabbit.tcp_listen_options.sndbuf = 16384
  * rabbit.tcp_listen_options.recbuf = 16384
* Don't use 32-bit Erlang

## Links

* [Docker image](https://hub.docker.com/_/rabbitmq)
* [Ruby client (bunny)](http://rubybunny.info/)
* [bunny tutorial](https://www.rabbitmq.com/tutorials/tutorial-one-ruby.html)
