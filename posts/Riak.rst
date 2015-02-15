Riak
----


Get stats
==============================
{{{
curl -H "Accept: text/plain" http://127.0.0.1:8098/stats
}}}
Add a server to Riak pool
==============================
{{{
riak-admin cluster join ${NODE}
}}}

