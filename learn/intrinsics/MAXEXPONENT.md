---
layout: book
title: maxexponent
permalink: /learn/intrinsics/MAXEXPONENT
---
## __Name__

__maxexponent__(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind
(GFDL)

## __Syntax__
```fortran
result = maxexponent(x)
```
## __Description__

__maxexponent(x)__ returns the maximum exponent in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_maxexponent
iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x
real(kind=dp) :: y

   print *, minexponent(x), maxexponent(x)
   print *, minexponent(y), maxexponent(y)
end program demo_maxexponent
```

## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
[fraction(3)](FRACTION),
[huge(3)](HUGE),
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

