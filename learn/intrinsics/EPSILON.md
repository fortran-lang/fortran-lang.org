---
layout: book
title: epsilon
permalink: /learn/intrinsics/EPSILON
---
## __Name__

__epsilon__(3) - \[NUMERIC MODEL\] Epsilon function
(GFDL)

## __Syntax__

result = __epsilon__(x)

## __Description__

__epsilon__(x) returns a nearly negligible number relative to 1.

## __Arguments__

  - __X__
    : The type shall be _real_.

## __Returns__

The return value is of same type as the argument.

## __Examples__

Sample program:

```fortran
program demo_epsilon
implicit none
real :: x = 3.143
real(8) :: y = 2.33
   print *, epsilon(x)
   print *, epsilon(y)
end program demo_epsilon
```
## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
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
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

