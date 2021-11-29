---
layout: book
title: fraction
permalink: /learn/intrinsics/FRACTION
---
## __Name__

__fraction__(3) - \[MODEL\_COMPONENTS\] Fractional part of the model representation
(GFDL)

## __Description__

__fraction__(x) returns the fractional part of the model representation
of X.

## __Syntax__

y = __fraction__(x)

## __Arguments__

  - __X__
    The type of the argument shall be a _real_.

## __Returns__

The return value is of the same type and kind as the argument. The
fractional part of the model representation of X is returned; it is x \*
__radix__(x)\*\*(__-exponent__(x)).

## __Examples__

Sample program:

```fortran
    program demo_fraction
    implicit none
      real :: x
      x = 178.1387e-4
      print *, fraction(x), x * radix(x)**(-exponent(x))
    end program demo_fraction
```

## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
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

