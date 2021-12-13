---
layout: book
title: lge
permalink: /learn/intrinsics/LGE
---
## __Name__

__lge__(3) - \[CHARACTER:COMPARE\] Lexical greater than or equal
(GFDL)

## __Syntax__
```fortran
result = lge(string_a, string_b)
```
## __Description__

Determines whether one string is lexically greater than or equal to
another string, where the two strings are interpreted as containing
ASCII character codes. If the String __a__ and String __b__ are not the same
length, the shorter is compared as if spaces were appended to it to form
a value that has the same length as the longer.

In general, the lexical comparison intrinsics __lge__(3), __lgt__(3), __lle__(3), and __llt__(3)
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \>= string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

__\[\[lgt__(3), __\[\[lle__(3), __\[\[llt__(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
