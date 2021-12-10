---
layout: book
title: scale
permalink: /learn/intrinsics/SCALE
---
## __Name__

__scale__(3) - \[MODEL\_COMPONENTS\] Scale a real value
(GFDL)

## __Syntax__
```fortran
result = scale(x, i)
```
## __Description__

__scale(x,i)__ returns x \* __radix(x)\*\*i__.

## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

  - __i__
    : The type of the argument shall be a _integer_.

## __Returns__

The return value is of the same type and kind as __x__. Its value is __x \*
radix(x)\*\*i__.

## __Examples__

Sample program:

```fortran
program demo_scale
implicit none
real :: x = 178.1387e-4
integer :: i = 5
   print *, scale(x,i), x*radix(x)**i
end program demo_scale
```

Results:

```
    0.570043862      0.570043862
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
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

