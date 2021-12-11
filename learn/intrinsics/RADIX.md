---
layout: book
title: radix
permalink: /learn/intrinsics/RADIX
---
## __Name__

__radix__(3) - \[NUMERIC MODEL\] Base of a model number
(GFDL)

## __Syntax__
```fortran
result = radix(x)
```
## __Description__

__radix(x)__ returns the base of the model representing the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _integer_ or _real_

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
end program demo_radix
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
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

