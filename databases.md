# Databases



<!---
read Gray & Reuter
read Weikum and Vossen
--->

## Michael Stonebreaker

<http://slideshot.epfl.ch/play/suri_stonebraker>


## Column store

<!---
   read Monet paper on column executor vs row executor
   Ralph Kimball
 --->

50-100 times faster (according to Stonebraker) (data?)
Only reads the columns needed, not the entire store

## OLTP (Online Transaction Processing)


## High-Availability

CAP theorem - consistency, availability, partition tolerance

## Log-Structured Merge Trees

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

## DBA Responsibilities

* installs/configures/updates/migrates databases
* develops and implements a backup strategy
  * responsible for ensuring that the data can be recovered without losing transactions
* responsible for securing the databases
  * authentication (setting up user accounts)
  * authorization (ensuring permissions are correct)
  * auditing (who did what to the database)
* storage and capacity planning
* performance monitoring and tuning
* troubleshooting
* high availability
  * online backups
  * clustering
  * replication
  * standby servers
* Handling large DBs
  * table partitioning (Oracle)
  * federated databases (SQL Server)
  * replication (MySQL)
* an understanding of relational theory
* data extraction/transformation/loading (ETL)

## Interesting DBA problems

- [Gay Marriage: the database engineering perspective](http://qntm.org/gay)
# ODBC
Open Database Connectivity

<https://docs.microsoft.com/en-us/sql/odbc/microsoft-open-database-connectivity-odbc>

