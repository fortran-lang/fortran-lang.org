---
layout: book
title: exponent
permalink: /learn/intrinsics/EXPONENT
---
## __Name__

__exponent__(3) - \[MODEL\_COMPONENTS\] Exponent function
(GFDL)

## __Syntax__

result = __exponent__(x)

## __Description__

__exponent__(x) returns the value of the exponent part of X. If X is
zero the value returned is zero.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of type default _integer_.

## __Examples__

Sample program:

```fortran
    program demo_exponent
    implicit none
      real :: x = 1.0
      integer :: i
      i = exponent(x)
      print *, i
      print *, exponent(0.0)
    end program demo_exponent
```

## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[fraction(3)](FRACTION),
[huge(3)](HUGE),
[maxexponent(3)](MAXEXPONENT),
[minexponent(3)](MINEXPONENT),
[nearest(3)](NEAREST),
[precision(3)](PRECISION),
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

