# Databases
.. TODO
   read Gray & Reuter
   read Weikum and Vossen

Michael Stonebreaker
--------------------

<http://slideshot.epfl.ch/play/suri_stonebraker>


Column store
------------

.. TODO
   read Monet paper on column executor vs row executor
   Ralph Kimball
   
50-100 times faster (according to Stonebraker) (data?)
Only reads the columns needed, not the entire store

OLTP (Online Transaction Processing)
------------------------------------


High-Availability
-----------------

CAP theorem - consistency, availability, partition tolerance

Log-Structured Merge Trees
--------------------------

Record data and changes in immutable segments.
When data is inserted/changed, the top level fills up and its data is copied into a new segment (compaction)
LevelDB, Apache HBase, Hyperdex, Cassandra, RocksDB, WiredTiger, Riak

Advantages:

* immutable storage segments easily cached+backed up
* writes are append-only, no need for read
* fragmentation not an issue (or replaced by simpler problems)
* less wear on SSDs (can't update data in-place)


### Links

* <http://jepsen.io>


