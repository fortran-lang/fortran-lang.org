---
layout: book
title: exponent
permalink: /learn/intrinsics/EXPONENT
---
## __Name__

__exponent__(3) - \[MODEL\_COMPONENTS\] Exponent function
(GFDL)

## __Syntax__

result = __exponent__(x)

## __Description__

__exponent__(x) returns the value of the exponent part of X. If X is
zero the value returned is zero.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of type default _integer_.

## __Examples__

Sample program:

```
    program demo_exponent
    implicit none
      real :: x = 1.0
      integer :: i
      i = exponent(x)
      print *, i
      print *, exponent(0.0)
    end program demo_exponent
```

## __Standard__

Fortran 95 and later
