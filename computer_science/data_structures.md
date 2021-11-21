---
title: Data Structures
---

<!---
.. TODO - add more love, this could be really good
.. TODO - Trees
.. TODO - Graphs
-->

## Primitives

- boolean
- char
- float
- double
- int
- enumerated type

## Arrays

- Contiguous in RAM
- Faster indexing than linked lists -  Θ(1)

## Linked Lists

- Lists made of nodes that contain a data item and a pointer/reference to the next (and possibly previous) node
- Non-contiguous in RAM
- Requires scanning the list sequentially to find element (no random access) Θ(n)
- Faster insertion and removal than a dynamic array Θ(1) (plus search time if in the middle), where a dynamic array may need to reallocate its memory to stay contiguous.

## Stack

- Last in-first out

## Queue

- First in-first out

## Skip lists

- Hierarchical linked lists that allow for faster indexing and insert/delete (Θ(log n) average, Θ(n) worst case)


## Vectors

## Trees

## Graphs

## Hash Table


- A data structure used to implement associative arrays
- Can provide near-constant time insertion, deletion, and lookup
- Hash collisions can be handled by chaining, ie pointer to a linked list. They can also be handled by open addressing, where you use linear probing to check for the next empty spot (for the lookup, check the hash's array element, and then each after that until the value is found or you reach an empty element). You could also use quadratic probing as well (1 element ahead, 2 elements ahead, ...)
- A good hash can prevent needless collisions. Use all the information in the key. Combine the info by multiplying with prime numbers
- If load factor greater than 75%, double it. If less than 20%, halve it.