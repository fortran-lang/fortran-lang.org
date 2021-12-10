---
layout: book
title: radix
permalink: /learn/intrinsics/RADIX
---
## __Name__

__radix__(3) - \[NUMERIC MODEL\] Base of a model number
(GFDL)

## __Syntax__
```fortran
result = radix(x)
```
## __Description__

__radix(x)__ returns the base of the model representing the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _integer_ or _real_

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
end program demo_radix
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
[precision(3)](PRECISION),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

