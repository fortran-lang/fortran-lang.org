---
layout: book
title: precision
permalink: /learn/intrinsics/PRECISION
---
## __Name__

__precision__(3) - \[NUMERIC MODEL\] Decimal precision of a real kind
(GFDL)

## __Syntax__

result = __precision__(x)

## __Description__

__precision__(x) returns the decimal precision in the model of the type
of X.

## __Arguments__

  - __X__
    Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
    program demo_precision
    implicit none
      real(kind=4) :: x(2)
      complex(kind=8) :: y

      print *, precision(x), range(x)
      print *, precision(y), range(y)
    end program demo_precision
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
[nearest(3)](NEAREST),
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

