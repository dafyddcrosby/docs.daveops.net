
# Databases

# What makes Big Data?

- Volume
- Variety
- Velocity
- Veracity


# Log-Structured Merge Trees

Record data and changes in immutable segments.

When data is inserted/changed, the top level fills up and its data is copied into a new segment (compaction)

LevelDB, Apache HBase, Hyperdex, Cassandra, RocksDB, WiredTiger, Riak

Advantages:

- immutable storage segments easily cached+backed up
- writes are append-only, no need for read
- fragmentation not an issue (or replaced by simpler problems)
- less wear on SSDs (can't update data in-place)


# Jepsen

- <http://jepsen.io>


## DBA Responsibilities

- installs/configures/updates/migrates databases
- develops and implements a backup strategy
    - responsible for ensuring that the data can be recovered without losing transactions

- responsible for securing the databases
    - authentication (setting up user accounts)
    - authorization (ensuring permissions are correct)
    - auditing (who did what to the database)

- storage and capacity planning
- performance monitoring and tuning
- troubleshooting
- high availability
    - online backups
    - clustering
    - replication
    - standby servers

- Handling large DBs
    - table partitioning (Oracle)
    - federated databases (SQL Server)
    - replication (MySQL)

- an understanding of relational theory
- data extraction/transformation/loading (ETL)


## Interesting DBA problems

- [Gay Marriage: the database engineering perspective](http://qntm.org/gay)


# ODBC Open Database Connectivity

<https://docs.microsoft.com/en-us/sql/odbc/microsoft-open-database-connectivity-odbc>


# Cassandra

Based on the [Amazon Dynamo](https://www.allthingsdistributed.com/2007/10/amazons_dynamo.html) paper

- Ring topology
- peer-to-peer, gossip protocol
- no special nodes
- Distributed Hash Table
- eventually consistent, tunable

```
Use vnodes whenever possible to avoid issues with topology changes, node rebuilds, hotspots, and heterogeneous clusters.
```

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

- Murmur3Partitioner: even distribution of data across the cluster using the MurmurHash algorithm.
- RandomPartitioner: default prior to 1.2. Uses MD5 hashes. If you don't use vnodes, you have to calculate the tokens.
- ByteOrderedPartitioner: orders rows lexically by key bytes. Not recommended, since it's hard to load balance and can have hot spots.


## ports

| desc  | port |
|----- |---- |
| cqlsh | 9042 |


## nodetool

<https://docs.datastax.com/en/cassandra/2.1/cassandra/tools/toolsNodetool_r.html>

```shell
#  Checking node repair
nodetool netstats
nodetool compactionstats
```


## Topologies

Uses 'snitches'

- SimpleSnitch (default, good for 1 DC)
- RackInferringSnitch (infers from IP addr: 10.DC.RACK.NODE)
- PropertyFileSnitch (a KV file of IP=DC:RACK)
- EC2Snitch (discovers AWS AZ/regions)


## Per-Query Consistency

```
ANY/ONE/QUORUM/LOCAL_QUORUM/ALL
```


## Quick Docker dev setup

```shell
# Run server instance
docker run --name some-cassandra -v ~/my/own/datadir:/var/lib/cassandra -d cassandra:latest
```


## Types

| name      | desc                       |
|--------- |-------------------------- |
| text      | UTF8                       |
| ascii     | ASCII                      |
| int       | 32-bit integer             |
| bigint    | 64-bit integer             |
| varint    | arbitrary size             |
| float     | 32-bit float               |
| double    | 64-bit float               |
| decimal   | variable-precision decimal |
| boolean   | boolean                    |
| timestamp | 'yyyy-mm-dd HH:mm:ssZ'     |
| uuid      | UUID v1 and v4             |
| blob      | binary (prefix with 0x)    |


## Resources

- Cassandra High Availability by X


## Links

- [Docker image](https://hub.docker.com/_/cassandra)
- [Ruby driver](https://github.com/datastax/ruby-driver)


# CouchDB


## get list of databases

```
curl localhost:5984/_all_dbs
```


## get database details

```
curl localhost:5984/db_name
```


## access futon

```
curl localhost:5984/_utils
```


## Links

- [Docker image](https://hub.docker.com/_/couchdb)


# Couchbase


## Links

- [Docker image](https://hub.docker.com/_/couchbase)
- [Ruby client](https://github.com/couchbase/couchbase-ruby-client)


# Elasticsearch


## Running in Docker

```shell
docker pull docker.elastic.co/elasticsearch/elasticsearch:7.10.0

# single node
docker run -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:7.10.0
```


## Links

- <https://www.elastic.co/guide/en/elasticsearch/reference/current/docker.html>
- [Kibana docker image](https://hub.docker.com/_/kibana)
- [Elasticsearch docker image](https://hub.docker.com/_/elasticsearch)


# MongoDB


## Queries

```
// Show current operations
db.currentOp()
// Show long running queries
db.currentOp()['inprog'].filter(function(x) {return x.secs_running > 10})
// Kill a query
db.killOp(12345)
```


## Administration

```
// Show database list
show dbs

// Create/switch to database
use myDb

// Drop database
db.dropDatabase();
```


## Rotate logs

```shell
kill -SIGUSR1
```


## Documentation / Links

- <http://docs.mongodb.org/manual/administration/production-notes/>
- [Docker image](https://hub.docker.com/_/mongo)
- [Ruby driver](https://github.com/mongodb/mongo-ruby-driver)


# OrientDB

<http://orientdb.com/>


# Time-series database


## influxdb

<https://www.influxdata.com/>


## Prometheus

Official docker container: prom/prometheus

- <http://localhost:9090/graph>
- <http://localhost:9090/metrics>
