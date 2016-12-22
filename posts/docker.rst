Docker
======
:date: 2016-11-22

CLI
---
::

  # list all containers
  docker ps -a
  # remove a container
  docker rm [container_id|name]

Dockerfile syntax
-----------------
::

  FROM image_name:image_tag
  MAINTAINER person <person@example.org>
  RUN install_things
  CMD ['executable', '--to', '--run']
