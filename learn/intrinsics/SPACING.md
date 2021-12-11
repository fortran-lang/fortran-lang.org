---
layout: book
title: spacing
permalink: /learn/intrinsics/SPACING
---
## __Name__

__spacing__(3) - \[MODEL\_COMPONENTS\] Smallest distance between two numbers of a given type
(GFDL)

## __Syntax__
```fortran
result = spacing(x)
```
## __Description__

Determines the distance between the argument __x__ and the nearest adjacent
number of the same type.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The result is of the same type as the input argument __x__.

## __Examples__

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
   write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
end program demo_spacing
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
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__tiny__(3)](TINY)

