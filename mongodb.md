# MongoDB

## Queries

```javascript
// Show current operations
db.currentOp()
// Show long running queries
db.currentOp()['inprog'].filter(function(x) {return x.secs_running > 10})
// Kill a query
db.killOp(12345)
```

## Administration

```javascript
// Show database list
show dbs

// Create/switch to database
use myDb

// Drop database
db.dropDatabase();
```

## Rotate logs

 kill -SIGUSR1 <mongod pid>

## Documentation / Links

* <http://docs.mongodb.org/manual/administration/production-notes/>
* [Docker image](https://hub.docker.com/_/mongo)
* [Ruby driver](https://github.com/mongodb/mongo-ruby-driver)
