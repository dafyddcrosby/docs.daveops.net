---
title: Kubernetes
tags: ["containers"]
---

Master -> Node + Node processes

Deployment -> (service -> ) pod (on a node) -> containers/volumes

### Kubectl

#### Cluster

```bash
# get cluster info
kubectl cluster-info
# get list of nodes
kubectl get nodes
```

#### Deployments

```bash
# create a deployment
kubectl run NAME --image=...
# see deployments
kubectl get deployments
```

#### Pods
Think of a pod as a logical host

```bash
# list pods
kubectl get pods
# run a command on a pod
kubectl exec $POD_NAME COMMAND
# label a pod
kubectl label pod $POD_NAME app=v1
```

#### Networking

```bash
# Open a proxy to the internal network
kubectl proxy

kubectl expose TYPE/NAME

# Forward port 8081 to a deployment
kubectl port-forward deployment/foobar 8082
```

#### Logs

```bash
kubectl logs $POD_NAME
```

#### Services

A logical collection of pods

Type         | Description
---          | ---
ClusterIP    |
NodePort     |
LoadBalancer | A load balancer
ExternalName |

```bash
# list services
kubectl get services
# delete services
kubectl delete service
```

#### DaemonSets
Long-running processes, generally monitoring/logging. Can come up first, and go down safely at reboot/shutdown.	

#### Namespaces

```bash
# list namespaces
kubectl get namespaces
# get detailed namespace info
kubectl describe namespace NAMESPACE
# create context from namespace
kubectl config set-context # ...
# Switch context
kubectl config use-context NAMESPACE
# Get current context
kubectl config current-context
```

#### Secrets

```bash
# Create generic secret from literal value
kubectl create secret generic OBJECTNAME --from-literal=KEYNAME=LITERAL_VALUE
```

<https://kubernetes.io/docs/concepts/configuration/secret/>

#### Misc

```bash
# Check control plane health
kubectl get componentstatus
```

## Links

* <https://kubernetes.io/>
* [kubectl reference](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands)
* <https://kubernetes.io/docs/concepts/configuration/overview/#general-config-tips>
* [Kubernetes post-mortems](https://k8s.af/)
