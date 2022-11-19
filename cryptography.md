# Cryptography


## XOR


## Frequency analysis

Certain letters show up more frequently in natural language. This leads to cryptographic weakness in classic ciphers

```shell
grep -o . file | sort | uniq -c
```

"ETAOIN SHRDLU" is a phrase to get the 12 most common characters in English.

Common pairs are consonants TH and vowels EA. Others are OF, TO, IN, IT, IS, BE, AS, AT, SO, WE, HE, BY, OR, ON, DO, IF, ME, MY, UP.

Common pairs of repeated letters are SS, EE, TT, FF, LL, MM and OO.

Common triplets of text are THE, EST, FOR, AND, HIS, ENT or THA.


### Links

- <http://www.richkni.co.uk/php/crypta/freq.php>
- <https://en.wikipedia.org/wiki/Frequency_analysis>
- <https://crypto.interactive-maths.com/frequency-analysis-breaking-the-code.html>


# Cryptographic hashes

- [Lifetimes of cryptographic hash functions](http://valerieaurora.org/hash.html)


# `Dual_EC_DRBG`

[The Many Flaws of `Dual_EC_DRBG`](https://blog.cryptographyengineering.com/2013/09/18/the-many-flaws-of-dualecdrbg/) [The Strange Story of Extended Random](https://blog.cryptographyengineering.com/2017/12/19/the-strange-story-of-extended-random/)


# Cryptographic Hashing

<https://valerieaurora.org/hash.html>


# Cryptopals

<http://cryptopals.com>


## Set 1

Base 64 - RFC 4648