---
layout: book
title: digits
permalink: /learn/intrinsics/DIGITS
---
## __Name__

__digits__(3) - \[NUMERIC MODEL\] Significant digits function
(GFDL)

## __Syntax__
```fortran
result = digits(x)
```
## __Description__

__digits(x)__ returns the number of significant digits of the internal
model representation of __x__. For example, on a system using a 32-bit
floating point representation, a default real number would likely return
24.

## __Arguments__

  - __x__
    : The type may be _integer_ or _real_.

## __Returns__

The return value is of type _integer_.

## __Examples__

Sample program:

```fortran
program demo_digits
implicit none
integer :: i = 12345
real :: x = 3.143
doubleprecision :: y = 2.33d0
   print *,'default integer:', digits(i)
   print *,'default real:   ', digits(x)
   print *,'default doubleprecision:', digits(y)
end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

## __Standard__

Fortran 95 and later

## __See Also__

[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
