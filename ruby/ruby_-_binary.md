# Ruby - Binary

## ::Array#pack / String#unpack

### Integer


Directive | Meaning
---       | ---
C         | 8-bit unsigned (unsigned char)
S         | 16-bit unsigned, native endian (uint16_t)
L         | 32-bit unsigned, native endian (uint32_t)
Q         | 64-bit unsigned, native endian (uint64_t)
c         | 8-bit signed (signed char)
s         | 16-bit signed, native endian (int16_t)
l         | 32-bit signed, native endian (int32_t)
q         | 64-bit signed, native endian (int64_t)
S_, S!    | unsigned short, native endian
I, I_, I! | unsigned int, native endian
L_, L!    | unsigned long, native endian
n         | 16-bit unsigned, network (big-endian) byte order
N         | 32-bit unsigned, network (big-endian) byte order
v         | 16-bit unsigned, VAX (little-endian) byte order
V         | 32-bit unsigned, VAX (little-endian) byte order
U         | UTF-8 character
w         | BER-compressed integer (see Array.pack)
Q_, Q!    | unsigned long long, native endian (ArgumentError if the platform has no long long type.)
s_, s!    | signed short, native endian
i, i_, i! | signed int, native endian
l_, l!    | signed long, native endian
q_, q!    | signed long long, native endian (ArgumentError if the platform has no long long type.)


desc          | suffix
---           | ---
native endian | !
native endian | _
big-endian    | >
little-endian | <

- J, J! j, and j! are available since Ruby 2.3.
- Q_, Q!, q_, and q! are available since Ruby 2.1.
- I!<, i!<, I!>, and i!> are available since Ruby 1.9.3.

### Float

Directive | Meaning
---       | ---
D, d      | double-precision, native format
F, f      | single-precision, native format
E         | double-precision, little-endian byte order
e         | single-precision, little-endian byte order
G         | double-precision, network (big-endian) byte order
g         | single-precision, network (big-endian) byte order

### String

Directive | Meaning
---       | ---
A         | arbitrary binary string (remove trailing nulls and ASCII spaces)
a         | arbitrary binary string
Z         | null-terminated string
B         | bit string (MSB first)
b         | bit string (LSB first)
H         | hex string (high nibble first)
h         | hex string (low nibble first)
u         | UU-encoded string
M         | quoted-printable, MIME encoding (:RFC:`2045`)
m         | base64 encoded string (:RFC:`2045`) (default)  base64 encoded string (:RFC:`4648`) if followed by 0
P         | pointer to a structure (fixed-length string)
p         | pointer to a null-terminated string

### Misc

Directive | Meaning
---       | ---
@         | skip to the offset given by the length argument
X         | skip backward one byte
x         | skip forward one byte



