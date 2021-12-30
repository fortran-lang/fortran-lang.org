---
layout: book
title: merge_bits
permalink: /learn/intrinsics/MERGE_BITS
---
## __Name__

__merge\_bits__(3) - \[BIT:COPY\] Merge of bits under mask


## __Syntax__
```fortran
result = mergebits(i, j, mask)
```
## __Description__

__merge\_bits(i, j, mask)__ merges the bits of __i__ and __j__ as determined by
the mask. The k-th bit of the result is equal to the k-th bit of __i__ if
the k-th bit of __mask__ is __1__; it is equal to the k-th bit of __j__ otherwise.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_ and of the same kind as __i__.

  - __mask__
    : Shall be of type _integer_ and of the same kind as __i__.

## __Returns__

The result is of the same type and kind as __i__.

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
