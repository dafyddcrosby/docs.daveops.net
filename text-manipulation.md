
# text manipulation

# awk


## Get a specific column

```shell
# print second column
awk '{print $2}'
```


# sed


## Delete all lines matching a pattern

```shell
sed -i '/pattern to match/d' ./infile
```


## Prepend a file

```shell
sed -i '1i header line' ./infile
```
