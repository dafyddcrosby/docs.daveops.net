Cassandra
=========
:date: 2016-05-27

Based on the Amazon Dynamo paper
Ring topology
peer-to-peer, gossip protocol
no special nodes
Distributed Hash Table
eventually consistent, tunable

::

  Use vnodes whenever possible to avoid issues with topology changes, node rebuilds, hotspots, and heterogeneous clusters.

vnodes mean more ranges, which makes it easier to give a range to a new node

Partitioners
------------
- Murmur3Partitioner: even distribution of data across the cluster using the MurmurHash algorithm.
- RandomPartitioner: default prior to 1.2. Uses MD5 hashes. If you don't use vnodes, you have to calculate the tokens.
- ByteOrderedPartitioner: orders rows lexically by key bytes. Not recommended, since it's hard to load balance and can have hot spots.

cqlsh
-----

port 9042

nodetool
--------

https://docs.datastax.com/en/cassandra/2.1/cassandra/tools/toolsNodetool_r.html

::

  # Checking node repair
  nodetool netstats
  nodetool compactionstats

Topologies
----------

Uses 'snitches'

- SimpleSnitch (default, good for 1 DC)
- RackInferringSnitch (infers from IP addr: 10.DC.RACK.NODE)
- PropertyFileSnitch (a KV file of IP=DC:RACK)
- EC2Snitch (discovers AWS AZ/regions)

Per-Query Consistency
---------------------

ANY/ONE/QUORUM/LOCAL_QUORUM/ALL

Resources
---------
- Cassandra High Availability by X

.. todo
   http://docs.datastax.com/en/cql/3.1/cql/ddl/ddl_when_use_index_c.html
   http://docs.datastax.com/en/cql/3.1/cql/ddl/ddl_intro_c.html
   http://www.datastax.com/dev/blog/new-in-cassandra-3-0-materialized-views
   http://www.datastax.com/dev/blog/2012-in-review-performance
   http://www.planetcassandra.org/try-cassandra/
   http://www.planetcassandra.org/blog/cassandra-native-secondary-index-deep-dive/
   https://www.instaclustr.com/blog/2016/01/27/apache-cassandra-compaction/
   https://tobert.github.io/pages/als-cassandra-21-tuning-guide.html
