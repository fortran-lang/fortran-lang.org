---
layout: book
title: nearest
permalink: /learn/intrinsics/NEAREST
---
## __Name__

__nearest__(3) - \[MODEL\_COMPONENTS\] Nearest representable number
(GFDL)

## __Syntax__

result = __nearest__(x, s)

## __Description__

__nearest__(x, s) returns the processor-representable number nearest to
X in the direction indicated by the sign of S.

## __Arguments__

  - __X__
    Shall be of type _real_.

  - __S__
    Shall be of type _real_ and not equal to zero.

## __Returns__

The return value is of the same type as X. If S is positive, NEAREST
returns the processor-representable number greater than X and nearest to
it. If S is negative, NEAREST returns the processor-representable number
smaller than X and nearest to it.

## __Examples__

Sample program:

```fortran
   program demo_nearest
   implicit none
     real :: x, y
     x = nearest(42.0, 1.0)
     y = nearest(42.0, -1.0)
     write (*,"(3(g20.15))") x, y, x - y
   end program demo_nearest
```

## __Standard__

Fortran 95 and later
