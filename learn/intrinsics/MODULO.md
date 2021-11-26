---
layout: book
title: modulo
permalink: /learn/intrinsics/MODULO
---
## __Name__

__modulo__(3) - \[NUMERIC\] Modulo function
(GFDL)

## __Syntax__

result = __modulo__(a, p)

## __Description__

__modulo__(a,p) computes the A modulo P.

## __Arguments__

  - __A__
    Shall be a scalar of type _integer_ or _real_.

  - __P__
    Shall be a scalar of the same type and kind as A. It shall not be
    zero.

## __Returns__

The type and kind of the result are those of the arguments.

  - If A and P are of type _integer_: __modulo__(a,p) has the value of a -
    floor (__real__(a) / __real__(p)) \* p.

  - If A and P are of type _real_: __modulo__(a,p) has the value of a -
    floor (a / p) \* p.

The returned value has the same sign as P and a magnitude less than the
magnitude of P.

## __Examples__

Sample program:

```
   program demo_modulo
   implicit none
     print *, modulo(17,3)        ! yields 2
     print *, modulo(17.5,5.5)    ! yields 1.0

     print *, modulo(-17,3)       ! yields 1
     print *, modulo(-17.5,5.5)   ! yields 4.5

     print *, modulo(17,-3)       ! yields -1
     print *, modulo(17.5,-5.5)   ! yields -4.5
   end program demo_modulo
```

## __Standard__

Fortran 95 and later

## __See Also__

__mod__(3)
