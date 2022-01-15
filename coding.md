# Coding

The focus of this page is the actual implementation of logic

# Binary Hacks

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


# Demoscene tricks

## Fixed point math

Take a 16-bit integer, and break it into two 8 bit parts, an integer part and a fractional part.
Addition and subtraction are the same, but for multiplication and division you will need to do bit shifting

	A = ( C * D ) >> 8
	A = ( C << 8 ) / D


## Interpolation

k is within 0..1

	C = A + (B-A) * k

