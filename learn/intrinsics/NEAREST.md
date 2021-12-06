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

## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
[fraction(3)](FRACTION),
[huge(3)](HUGE),
[maxexponent(3)](MAXEXPONENT),
[minexponent(3)](MINEXPONENT),
[precision(3)](PRECISION),
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

