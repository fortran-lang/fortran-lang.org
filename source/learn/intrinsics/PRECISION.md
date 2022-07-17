---
layout: book
title: precision
permalink: /learn/intrinsics/PRECISION
---
# PRECISION
## __Name__

__precision__(3) - \[NUMERIC MODEL\] Decimal precision of a real kind


## __Syntax__
```fortran
result = precision(x)
```
## __Description__

__precision(x)__ returns the decimal precision in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_precision
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x(2)
complex(kind=dp) :: y

   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_precision
```
  Results:
```text
              6          37
             15         307
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
