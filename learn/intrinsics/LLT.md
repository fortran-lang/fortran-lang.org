---
layout: book
title: llt
permalink: /learn/intrinsics/LLT
---
#### NAME

__llt__(3f) - \[CHARACTER\] Lexical less than
(GFDL)

#### SYNTAX

result = __llt__(string\_a, string\_b)

#### DESCRIPTION

Determines whether one string is lexically less than another string,
where the two strings are interpreted as containing ASCII character
codes. If the String A and String B are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

#### ARGUMENTS

  - __string\_a__
    Shall be of default CHARACTER type.

  - __string\_b__
    Shall be of default CHARACTER type.

#### RETURN VALUE

Returns .true. if string\_a \<= string\_b, and .false. otherwise, based
on the ASCII ordering.

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__\[\[lge__(3), __\[\[lgt__(3), __\[\[lle__(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
