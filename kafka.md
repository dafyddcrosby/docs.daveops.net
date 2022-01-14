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

