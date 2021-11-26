---
layout: book
title: minexponent
permalink: /learn/intrinsics/MINEXPONENT
---
## __Name__

__minexponent__(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind
(GFDL)

## __Syntax__

result = __minexponent__(x)

## __Description__

__minexponent__(x) returns the minimum exponent in the model of the type
of X.

## __Arguments__

  - __X__
    Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```
    program demo_minexponent
    implicit none
    real(kind=4) :: x
    real(kind=8) :: y
       print *, minexponent(x), maxexponent(x)
       print *, minexponent(y), maxexponent(y)
    end program demo_minexponent
```

## __Standard__

Fortran 95 and later
