---
layout: book
title: tiny
permalink: /learn/intrinsics/TINY
---
# TINY
## __Name__

__tiny__(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind


## __Syntax__
```fortran
result = tiny(x)
   real(kind=KIND) function(x)
   real(kind=KIND) :: x
```
  where KIND may be any kind supported by type _real_

## __Description__

__tiny(x)__ returns the smallest positive (non zero) number of the type
and kind of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The smallest positive value for the _real_ type of the specified kind.

The return value is of the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tiny
implicit none
   print *, 'default real is from',tiny(0.0) ,'to',huge(0.0)
   print *, 'doubleprecision is from ',tiny(0.0d0),'to',huge(0.0d0)
end program demo_tiny
```
Results:
```text
 default real is from 1.17549435E-38 to 3.40282347E+38
 doubleprecision is from 2.2250738585072014E-308 to 1.7976931348623157E+308
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
[__set_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING)

###### fortran-lang intrinsic descriptions
