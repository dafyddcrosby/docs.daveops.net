# Workload Orchestration

# Twine

- https://engineering.fb.com/2019/06/06/data-center-engineering/twine/


# Kubernetes
- <https://kubernetes.io/>
- <https://kubernetes.io/docs/concepts/configuration/overview/#general-config-tips>
- [Kubernetes post-mortems](https://k8s.af/)

Master -> Node + Node processes

Deployment -> (service -> ) pod (on a node) -> containers/volumes

# Kubectl
[kubectl reference](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands)

## Cluster

```bash
# get cluster info
kubectl cluster-info
# get list of nodes
kubectl get nodes
```

## Deployments

```bash
# create a deployment
kubectl run NAME --image=...
# see deployments
kubectl get deployments
```

## Pods
Think of a pod as a logical host

```bash
# list pods
kubectl get pods
# run a command on a pod
kubectl exec $POD_NAME COMMAND
# label a pod
kubectl label pod $POD_NAME app=v1
```

## Networking

```bash
# Open a proxy to the internal network
kubectl proxy

kubectl expose TYPE/NAME

# Forward port 8081 to a deployment
kubectl port-forward deployment/foobar 8082
```

## Logs

```bash
kubectl logs $POD_NAME
```

## Services

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

## DaemonSets
Long-running processes, generally monitoring/logging. Can come up first, and go down safely at reboot/shutdown.	

## Namespaces

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

## Secrets

```bash
# Create generic secret from literal value
kubectl create secret generic OBJECTNAME --from-literal=KEYNAME=LITERAL_VALUE
```

<https://kubernetes.io/docs/concepts/configuration/secret/>

## Misc

```bash
# Check control plane health
kubectl get componentstatus
```



# minikube
<https://github.com/kubernetes/minikube>

Master -> Node -> Node processes

	minikube start



# k3
* https://github.com/rancher/k3s
* https://github.com/rancher/k3d


# k3s
* https://github.com/rancher/k3s
* https://github.com/rancher/k3d


# metrics-server

<https://github.com/kubernetes-incubator/metrics-server>
<https://kubernetes.io/docs/tasks/debug-application-cluster/core-metrics-pipeline/>



# heapster

<https://github.com/kubernetes/heapster>
deprecated Kubernetes 1.11, retired in Kubernetes 1.13
collects metrics from cadvisor
to be replaced by [[kubernetes]]


# cadvisor

<https://hub.docker.com/r/google/cadvisor/>
collects resource/perf metrics from running containers
UI - port was 4194
in Kubernetes 1.11, UI turned off by default



# Mesos
<https://mesos.apache.org/>



# Nomad


```bash
# view registered nodes
nomad node status

# list servers
nomad server members

# Start a dev server
sudo nomad agent -dev

# run a job
nomad run JOBNAME.nomad
```

Default web UI port is 4646

- [CLI reference](https://www.nomadproject.io/docs/commands/index.html)
