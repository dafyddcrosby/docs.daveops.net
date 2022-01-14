# Elasticsearch

## Running in Docker

```bash
docker pull docker.elastic.co/elasticsearch/elasticsearch:7.10.0

# single node
docker run -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:7.10.0
```

## Links

* https://www.elastic.co/guide/en/elasticsearch/reference/current/docker.html
* [Kibana docker image](https://hub.docker.com/_/kibana)
* [Elasticsearch docker image](https://hub.docker.com/_/elasticsearch)
