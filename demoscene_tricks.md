# Demoscene tricks

## Fixed point math

Take a 16-bit integer, and break it into two 8 bit parts, an integer part and a fractional part.
Addition and subtraction are the same, but for multiplication and division you will need to do bit shifting

	A = ( C * D ) >> 8
	A = ( C << 8 ) / D


## Interpolation

k is within 0..1

	C = A + (B-A) * k

