---
layout: book
title: sign
permalink: /learn/intrinsics/SIGN
---
## __Name__

__sign__(3) - \[NUMERIC\] Sign copying function
(GFDL)

## __Syntax__

result = __sign__(a, b)

## __Description__

__sign__(a,b) returns the value of A with the sign of B.

## __Arguments__

  - __A__
    Shall be of type _integer_ or _real_

  - __B__
    Shall be of the same type and kind as A

## __Returns__

The kind of the return value is that of A and B. If B \>= 0 then the
result is __abs__(a), else it is -__abs__(a).

## __Examples__

Sample program:

```fortran
    program demo_sign
    implicit none
      print *, sign(-12,1)
      print *, sign(-12,0)
      print *, sign(-12,-1)

      print *, sign(-12.,1.)
      print *, sign(-12.,0.)
      print *, sign(-12.,-1.)
    end program demo_sign
```

## __Standard__

FORTRAN 77 and later
