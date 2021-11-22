---
layout: book
title: merge_bits
permalink: /learn/intrinsics/f_merge_bits
---
### NAME

**merge\_bits**(3f) - \[BIT MANIPULATION\] Merge of
bits under mask

### SYNTAX

result = **merge\_bits**(i, j, mask)

### DESCRIPTION

**merge\_bits**(i, j, mask) merges the bits of I and J as determined by
the mask. The k-th bit of the result is equal to the k-th bit of I if
the k-th bit of MASK is 1; it is equal to the k-th bit of J otherwise.

### ARGUMENTS

  - **I**
    Shall be of type INTEGER.

  - **J**
    Shall be of type INTEGER and of the same kind as I.

  - **MASK**
    Shall be of type INTEGER and of the same kind as I.

### RETURN VALUE

The result is of the same type and kind as I.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function
