---
layout: book
title: merge_bits
permalink: /learn/intrinsics/MERGE_BITS
---
## __Name__

__merge\_bits__(3) - \[BIT MANIPULATION\] Merge of bits under mask
(GFDL)

## __Syntax__

result = __merge\_bits(i, j, mask)__

## __Description__

__merge\_bits(i, j, mask)__ merges the bits of __I__ and __J__ as determined by
the mask. The k-th bit of the result is equal to the k-th bit of __I__ if
the k-th bit of __MASK__ is __1__; it is equal to the k-th bit of __J__ otherwise.

## __Arguments__

  - __I__
    Shall be of type _integer_.

  - __J__
    Shall be of type _integer_ and of the same kind as __I__.

  - __MASK__
    Shall be of type _integer_ and of the same kind as __I__.

## __Returns__

The result is of the same type and kind as __I__.

## __Standard__

Fortran 2008 and later
