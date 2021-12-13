---
layout: book
title: nearest
permalink: /learn/intrinsics/NEAREST
---
## __Name__

__nearest__(3) - \[MODEL\_COMPONENTS\] Nearest representable number
(GFDL)

## __Syntax__
```fortran
result = nearest(x, s)
```
## __Description__

__nearest(x, s)__ returns the processor-representable number nearest to
__x__ in the direction indicated by the sign of __s__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

  - __s__
    : Shall be of type _real_ and not equal to zero.

## __Returns__

The return value is of the same type as __x__. If __s__ is positive, __nearest__
returns the processor-representable number greater than __x__ and nearest to
it. If __s__ is negative, __nearest__ returns the processor-representable number
smaller than __x__ and nearest to it.

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
  Results:
```text
   42.0000038146973    41.9999961853027    .762939453125000E-05
```
## __Standard__

Fortran 95 and later
## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

