---
layout: book
title: fraction
permalink: /learn/intrinsics/FRACTION
---
## __Name__

__fraction__(3) - \[MODEL\_COMPONENTS\] Fractional part of the model representation
(GFDL)

## __Description__

__fraction__(x) returns the fractional part of the model representation
of X.

## __Syntax__

y = __fraction__(x)

## __Arguments__

  - __X__
    The type of the argument shall be a _real_.

## __Returns__

The return value is of the same type and kind as the argument. The
fractional part of the model representation of X is returned; it is x \*
__radix__(x)\*\*(__-exponent__(x)).

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

## __Standard__

Fortran 95 and later
