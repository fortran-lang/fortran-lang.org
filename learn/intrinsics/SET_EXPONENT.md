---
layout: book
title: set_exponent
permalink: /learn/intrinsics/SET_EXPONENT
---
## __Name__

__set\_exponent__(3) - \[MODEL\_COMPONENTS\] Set the exponent of the model
(GFDL)

## __Syntax__

result = __set\_exponent__(x, i)

## __Description__

__set\_exponent__(x, i) returns the real number whose fractional part is
that of X and whose exponent part is I.

## __Arguments__

  - __X__
    : Shall be of type _real_.

  - __I__
    : Shall be of type _integer_.

## __Returns__

The return value is of the same type and kind as X. The real number
whose fractional part is that that of X and whose exponent part if I is
returned; it is __fraction__(x) \* __radix__(x)\*\*i.

## __Examples__

Sample program:

```fortran
program demo_setexp
implicit none
real :: x = 178.1387e-4
integer :: i = 17
   print *, set_exponent(x, i), fraction(x) * radix(x)**i
end program demo_setexp
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
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[spacing(3)](SPACING),
[tiny(3)](TINY)

