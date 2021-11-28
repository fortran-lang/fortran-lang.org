---
layout: book
title: range
permalink: /learn/intrinsics/RANGE
---
## __Name__

__range__(3) - \[NUMERIC MODEL\] Decimal exponent range of a real kind
(GFDL)

## __Syntax__

result = __range__(x)

## __Description__

__range__(x) returns the decimal exponent range in the model of the type
of X.

## __Arguments__

  - __X__
    Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
    program demo_range
    implicit none
    real(kind=4) :: x(2)
    complex(kind=8) :: y
       print *, precision(x), range(x)
       print *, precision(y), range(y)
    end program demo_range
```

## __Standard__

Fortran 95 and later

## __See Also__

__selected\_real\_kind__(3), __precision__(3)
