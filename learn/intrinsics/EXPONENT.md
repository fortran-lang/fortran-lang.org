---
layout: book
title: exponent
permalink: /learn/intrinsics/EXPONENT
---
## __Name__

__exponent__(3) - \[MODEL\_COMPONENTS\] Exponent function
(GFDL)

## __Syntax__
```fortran
result = exponent(x)
```
## __Description__

__exponent__(x) returns the value of the exponent part of __x__. If __x__ is
zero the value returned is zero.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type default _integer_.

## __Examples__

Sample program:

```fortran
program demo_exponent
implicit none
real :: x = 1.0
integer :: i
   i = exponent(x)
   print *, i
   print *, exponent(0.0)
end program demo_exponent
```
  Results:
```text
              1
              0
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
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
