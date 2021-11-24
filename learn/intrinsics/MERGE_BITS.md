---
layout: book
title: merge_bits
permalink: /learn/intrinsics/MERGE_BITS
---
#### NAME

__merge\_bits__(3f) - \[BIT MANIPULATION\] Merge of bits under mask
(GFDL)

#### SYNTAX

result = __merge\_bits__(i, j, mask)

#### DESCRIPTION

__merge\_bits__(i, j, mask) merges the bits of I and J as determined by
the mask. The k-th bit of the result is equal to the k-th bit of I if
the k-th bit of MASK is 1; it is equal to the k-th bit of J otherwise.

#### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

  - __J__
    Shall be of type INTEGER and of the same kind as I.

  - __MASK__
    Shall be of type INTEGER and of the same kind as I.

#### RETURN VALUE

The result is of the same type and kind as I.

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental function
