---
layout: book
title: maskl
permalink: /learn/intrinsics/MASKL
---
## __Name__

__maskl__(3) - \[\] Left justified mask
(GFDL)
## __Syntax__
```fortran
result = maskl(i, kind)__
```
## __Description__

__maskl(i\[, *kind*\])__ has its leftmost __I__ bits set to __1__, and the
remaining bits set to __0__.

## __Arguments__

  - __I__
    : Shall be of type _integer_.

  - __KIND__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __KIND__ is present, it specifies
the *kind* value of the return type; otherwise, it is of the default
integer *kind*.

## __Standard__

Fortran 2008 and later

## __See Also__

[__maskr__(3)](MASKR)
