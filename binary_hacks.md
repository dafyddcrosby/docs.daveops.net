---
title: Binary Hacks
---

## Determine if integer is odd
```c
if ((x & 1) == 0) {
  //x is even
} else {
  //x is odd
}
```

## Determine if nth bit is set

```c
if (x & (1<<n)) {
  // n-th bit is set
} else {
  //  n-th bit is not set
}
```

## Set the nth bit

```c
y = x | (1<<n)
```


## Unset the nth bit

```c
y = x & ~(1<<n)
```


## Toggle the nth bit

```c
y = x ^ (1<<n)
```


