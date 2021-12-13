---
layout: book
title: fraction
permalink: /learn/intrinsics/FRACTION
---
## __Name__

__fraction__(3) - \[MODEL\_COMPONENTS\] Fractional part of the model representation
(GFDL)

## __Syntax__
```fortran
y = fraction(x)
```
## __Description__

__fraction(x)__ returns the fractional part of the model representation
of __x__.


## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

## __Returns__

The return value is of the same type and kind as the argument. The
fractional part of the model representation of __x__ is returned; it is 
__x \* radix(x)\*\*(-exponent(x))__.

## __Examples__

Sample program:

```fortran
program demo_fraction
implicit none
real :: x
   x = 178.1387e-4
   print *, fraction(x), x * radix(x)**(-exponent(x))
end program demo_fraction
```
  Results:
```text
     0.570043862      0.570043862    
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
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
