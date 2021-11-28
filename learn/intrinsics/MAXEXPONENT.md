---
layout: book
title: maxexponent
permalink: /learn/intrinsics/MAXEXPONENT
---
## __Name__

__maxexponent__(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind
(GFDL)

## __Syntax__

result = __maxexponent(X)__

## __Description__

__maxexponent(X)__ returns the maximum exponent in the model of the type
of X.

## __Arguments__

  - __X__
    Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
    program demo_maxexponent
    implicit none
      real(kind=4) :: x
      real(kind=8) :: y

      print *, minexponent(x), maxexponent(x)
      print *, minexponent(y), maxexponent(y)
    end program demo_maxexponent
```

## __Standard__

Fortran 95 and later
