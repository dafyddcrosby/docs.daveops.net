---
title: Cassandra
---

Based on the [Amazon Dynamo](https://www.allthingsdistributed.com/2007/10/amazons_dynamo.html) paper

Ring topology
peer-to-peer, gossip protocol
no special nodes
Distributed Hash Table
eventually consistent, tunable

	Use vnodes whenever possible to avoid issues with topology changes, node rebuilds, hotspots, and heterogeneous clusters.

vnodes mean more ranges, which makes it easier to give a range to a new node


## Keyspaces

List keyspaces

```
DESCRIBE KEYSPACES;
```

Create a keyspace

```
CREATE KEYSPACE "my_space"
WITH REPLICATION = {
  'class': 'SimpleStrategy', 'replication_factor': 1
};
```

Use a keyspace

```
USE "my_space";
```

## Tables

List tables

```
DESCRIBE TABLES;
```

Create table

```
CREATE TABLE "users" (
  "user" text PRIMARY KEY,
  "email" text,
  "avatar" blob
);
```

Insert into table

```
INSERT INTO "users"
("avatar", "email", "avatar")
VALUES (
  'admin',
  'admin@example.org',
  0xf00badc0ff33
);
```

## Partitioners

* Murmur3Partitioner: even distribution of data across the cluster using the MurmurHash algorithm.
* RandomPartitioner: default prior to 1.2. Uses MD5 hashes. If you don't use vnodes, you have to calculate the tokens.
* ByteOrderedPartitioner: orders rows lexically by key bytes. Not recommended, since it's hard to load balance and can have hot spots.

## ports

desc  | port
---   | ---
cqlsh | 9042

## nodetool

<https://docs.datastax.com/en/cassandra/2.1/cassandra/tools/toolsNodetool_r.html>

```bash
#  Checking node repair
nodetool netstats
nodetool compactionstats
```

## Topologies

Uses 'snitches'


* SimpleSnitch (default, good for 1 DC)
* RackInferringSnitch (infers from IP addr: 10.DC.RACK.NODE)
* PropertyFileSnitch (a KV file of IP=DC:RACK)
* EC2Snitch (discovers AWS AZ/regions)


## Per-Query Consistency

ANY/ONE/QUORUM/LOCAL_QUORUM/ALL

## Quick Docker dev setup

```bash
# Run server instance
docker run --name some-cassandra -v ~/my/own/datadir:/var/lib/cassandra -d cassandra:latest
```

## Types

name      | desc
---       | ---
text      | UTF8
ascii     | ASCII
int       | 32-bit integer
bigint    | 64-bit integer
varint    | arbitrary size
float     | 32-bit float
double    | 64-bit float
decimal   | variable-precision decimal
boolean   | boolean
timestamp | 'yyyy-mm-dd HH:mm:ssZ'
uuid      | UUID v1 and v4
blob      | binary (prefix with 0x)

## Resources

* Cassandra High Availability by X

<!--
   <http://docs.datastax.com/en/cql/3.1/cql/ddl/ddl_when_use_index_c.html>
   <http://docs.datastax.com/en/cql/3.1/cql/ddl/ddl_intro_c.html>
   <http://www.datastax.com/dev/blog/new-in-cassandra-3-0-materialized-views>
   <http://www.datastax.com/dev/blog/2012-in-review-performance>
   <http://www.planetcassandra.org/try-cassandra/>
   <http://www.planetcassandra.org/blog/cassandra-native-secondary-index-deep-dive/>
   <https://www.instaclustr.com/blog/2016/01/27/apache-cassandra-compaction/>
   <https://tobert.github.io/pages/als-cassandra-21-tuning-guide.html>
-->

## Links

* [Docker image](https://hub.docker.com/_/cassandra)
* [Ruby driver](https://github.com/datastax/ruby-driver)
