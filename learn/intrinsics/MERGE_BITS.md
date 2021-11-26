---
layout: book
title: merge_bits
permalink: /learn/intrinsics/MERGE_BITS
---
## __Name__

__merge\_bits__(3) - \[BIT MANIPULATION\] Merge of bits under mask
(GFDL)

## __Syntax__

result = __merge\_bits__(i, j, mask)

## __Description__

__merge\_bits__(i, j, mask) merges the bits of I and J as determined by
the mask. The k-th bit of the result is equal to the k-th bit of I if
the k-th bit of MASK is 1; it is equal to the k-th bit of J otherwise.

## __Arguments__

  - __I__
    Shall be of type _integer_.

  - __J__
    Shall be of type _integer_ and of the same kind as I.

  - __MASK__
    Shall be of type _integer_ and of the same kind as I.

## __Returns__

The result is of the same type and kind as I.

## __Standard__

Fortran 2008 and later
