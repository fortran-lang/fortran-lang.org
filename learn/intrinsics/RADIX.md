---
layout: book
title: radix
permalink: /learn/intrinsics/RADIX
---
## __Name__

__radix__(3) - \[NUMERIC MODEL\] Base of a model number
(GFDL)

## __Syntax__

result = __radix__(x)

## __Description__

__radix__(x) returns the base of the model representing the entity X.

## __Arguments__

  - __X__
    Shall be of type _integer_ or _real_

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

__scale__(3), __selected\_real\_kind__(3)
