---
layout: book
title: tiny
permalink: /learn/intrinsics/TINY
---
## __Name__

__tiny__(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind
(GFDL)

## __Syntax__

result = __tiny__(x)

## __Description__

__tiny__(x) returns the smallest positive (non zero) number in the model
of the type of X.

## __Arguments__

  - __X__
    Shall be of type _real_.

## __Returns__

The return value is of the same type and kind as X

## __Examples__

Sample program:

```
    program demo_tiny
    implicit none
      print *, huge(0), huge(0.0), huge(0.0d0)
      print *, tiny(0.0), tiny(0.0d0)
    end program demo_tiny
```

## __Standard__

Fortran 95 and later
