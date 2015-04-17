RabbitMQ
========
:date:

add/restart/remove cluster node
-------------------------------
::

 # Add a node
 rabbitmqctl stop_app
 rabbitmqctl join_cluster rabbit@rabbit2
 rabbitmqctl start_app

 # Restart a node
 rabbitmqctl stop
 rabbitmq-server -detached

 # Remove a node (locally)
 rabbitmqctl stop_app
 # Remove a node (remotely)
 rabbitmqctl forget_cluster_node rabbit@rabbit1
 

Rotate logs
-----------
::

 rabbitmqctl rotate_logs <suffix>
