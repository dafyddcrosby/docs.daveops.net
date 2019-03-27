---
title: Docker
tags: ["containers"]
---

Installing on Mac
-----------------
```bash
brew cask install docker
```

CLI
---
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

Docker compose
--------------

<https://docs.docker.com/compose/compose-file/>

Docker stack
------------

<https://docs.docker.com/engine/reference/commandline/stack/>

Dockerfile syntax
-----------------

Must use ", not '

```dockerfile
FROM image_name:image_tag
MAINTAINER person <person@example.org>
RUN install_things
CMD ["executable", "--to", "--run"]
```
