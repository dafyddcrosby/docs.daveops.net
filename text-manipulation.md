# text manipulation

# awk

## Get a specific column

```bash
# print second column
awk '{print $2}'
```


# sed

## Delete all lines matching a pattern

```bash
sed -i '/pattern to match/d' ./infile
```

## Prepend a file

```bash
sed -i '1i header line' ./infile
```
