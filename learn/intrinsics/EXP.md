---
layout: book
title: exp
permalink: /learn/intrinsics/EXP
---
## __Name__

__exp__(3) - \[MATHEMATICS\] Exponential function
(GFDL)

## __Syntax__

result = __exp__(x)

## __Description__

__exp__(x) computes the base "e" exponential of X.

## __Arguments__

  - __X__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as X.

## __Examples__

Sample program:

```fortran
program demo_exp
implicit none
  real :: x = 1.0
  x = exp(x)
end program demo_exp
```

## __Standard__

FORTRAN 77 and later
