---
title: Docker
tags: ["containers"]
---

## Installing on Mac
```bash
brew cask install docker
```

## CLI
```bash
# list all containers
docker ps -a
# remove a container
docker rm [container_id|name]
# restart a container
docker restart [container]
# build a docker image from a Dockerfile
docker build -t TAGNAME . # in dir of Dockerfile
# create a docker container
docker create TAGNAME
# create and run a docker container, open a shell
docker run --name CONTAINERNAME -it
```

## Docker compose

```bash
docker-compose up
```

* <https://docs.docker.com/compose/>
* <https://docs.docker.com/compose/compose-file/>

## Docker stack

<https://docs.docker.com/engine/reference/commandline/stack/>

## Dockerfile syntax

Must use ", not '

```dockerfile
FROM image_name:image_tag
LABEL maintainer="person <person@example.org>"
RUN install_things
CMD ["executable", "--to", "--run"]
```

## Logs

```bash
docker logs <CONTAINER>
```

See also `/var/lib/docker/containers/<ID>/<ID>-json.log`

## Docker registry

* [Instructions](https://github.com/docker/docker.github.io/blob/master/registry/deploying.md)
* [Docker image](https://hub.docker.com/_/registry)

```bash
docker run -d -p 5000:5000 --restart=always --name registry registry:2
```
