# Distributed Systems
# Zookeeper

by default listens on port 2181/tcp

```text
# the port at which the clients will connect
clientPort=2181
# disable the per-ip limit on the number of connections if this is a non-production config
maxClientCnxns=0
```

## Links

* [Docker image](https://hub.docker.com/_/zookeeper)
* [Zookeeper paper](https://www.usenix.org/legacy/events/usenix10/tech/full_papers/Hunt.pdf)
# Kafka

by default listens on port 9092

## broker properties

property                       | desc
---                            | ---
broker.id                      | unique non-negative integer to be used as broker name
host.name                      | hostname of the broker
num.partitions                 | default number of partitions per topic
default.replication.factor     | default replication factor for topics
unclean.leader.election.enable | choose between consistency and availability in event of an unclean leader election


## kafka-topics

arg                      | desc
---                      | ---
--create TOPIC           | create a topic
--delete TOPIC           | delete a topic
--alter TOPIC            | alter partitions / replica assignment / configuration for a topic
--list                   | list the topics
--partitions NUM         | number of partitions for the topic being altered
--zookeeper URLS         | connection string for zookeeper connection (``host:port``)
--topic TOPIC            | The topic to be created/deleted/altered
--replication-factor NUM | The replication factor for each partition in the topic being created

## kafka-console-producer

```bash
kafka-console-producer --broker-list localhost:9092 --topic TOPIC
```

## kafka-console-consumer

```bash
kafka-console-consumer --zookeeper localhost:2181 --topic kafkatopic --from-beginning
```

## Links

* [Broker config](http://kafka.apache.org/documentation.html#brokerconfig)

## The Kafka Paper

* [Paper](http://notes.stephenholiday.com/Kafka.pdf)

Kafka used for the on-line consumption of logs

Pub-sub model, consumers pull

Message is a payload of bytes

Storage:
* segments of approximate uniformity (~1GB)
* message only exposed to consumers after flush
* message is logical offset in log (it will increase, but not consecutively)

leverages the filesystem page cache (ie no double buffering)

uses the Linux sendfile API call to avoid overhead

brokers use time-based retention (no knowledge of consumer state) - this allows consumer rewind

consumer groups - 1 consumer

partition in a topic is the smallest unit - to even the load, overpartition the topic

uses Zookeeper to coordinate (still?)

guarantees at-least-once delivery, not only-once delivery

no guarantee of ordering from different partitions

no replication at time of paper's writing

by keeping feature set minimal with small storage format, was more efficient than ActiveMQ/RabbitMQ in testing

# RabbitMQ

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

port  | desc
---   | ---
4369  | EPMD
5672  | AMQP connections
15672 | mgmt interface

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
# OpenLDAP

## ldapsearch

flag           | desc
---            | ---
-LLL           | LDIF, with no comments or version
-Z             | Use StartTLS
-H $HOST       | $HOST is server
-b $SEARCHBASE | $SEARCHBASE is start point for search
-x             | Simple auth (not SASL)
-W             | Prompt for simple auth
-D $BINDDN     | Use $BINDDN to bind to LDAP directory

## Links

* <http://www.openldap.org/>
* [Admin Guide (2.4)](http://www.openldap.org/doc/admin24/guide.html)

## RFCs
* 2251
* 4510-4519
# Kerberos

## Terminology

### Key Distribution Center 

Holds the key database, Authentication Server, Ticket Granting Server. This
might all be handled by one service.  It's super sensitive, treat is as such.
Since it handles authentication, at least one KDC should be running at all times

<!--
K:TDG says there's no standard synchronization mechanism
-->

### Authentication Server

Issues the Ticket Granting Ticket. Only the correct password decrypts the TGT.
Once the TGT is decrypted, it can be used to request individual service tickets.

The strength of the ticket is the strength of the password! (ie rotate PW
regularly, use hard passwords).

### Ticket Granting Server

Issues individual service tickets.

<!--
key version number (kvno) is important for services

krbtgt/REALM@REALM
-->

### Realm

A sort of namespace for principals (ie users, services).

## KRB4 vs. KRB5

### Kerberos 4
* Uses 56-bit DES (yikes!)

### Kerberos 5
* credential forwarding
* ASN.1

## Resources
* http://www.kerberos.info/
* *Kerberos: The Definitive Guide* by Jason Garman

<!--
Roger Needham / Michael Schroeder - "Using encryption for authentication in
large networks of computers"
  - Thought MITM was an "extreme view"
-->
# OwnCloud

## Links
* [Docker image](https://hub.docker.com/r/owncloud/server/)
