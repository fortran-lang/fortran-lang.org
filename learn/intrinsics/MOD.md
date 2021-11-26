---
layout: book
title: mod
permalink: /learn/intrinsics/MOD
---
## __Name__

__mod__(3) - \[NUMERIC\] Remainder function
(GFDL)

## __Syntax__

result = __mod__(a, p)

## __Description__

__mod__(a,p) computes the remainder of the division of A by P.

## __Arguments__

  - __A__
    Shall be a scalar of type _integer_ or _real_.

  - __P__
    Shall be a scalar of the same type and kind as A and not equal to
    zero.

## __Returns__

The return value is the result of a - (int(a/p) \* p). The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as A and a magnitude less than the magnitude of
P.

## __Examples__

Sample program:

```
   program demo_mod
   implicit none
     print *, mod(17,3)           ! yields 2
     print *, mod(17.5,5.5)       ! yields 1.0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

     print *, mod(-17,3)          ! yields -2
     print *, mod(-17.5,5.5)      ! yields -1.0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

     print *, mod(17,-3)          ! yields 2
     print *, mod(17.5,-5.5)      ! yields 1.0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
   end program demo_mod
```

## __Standard__

FORTRAN 77 and later

## __See Also__

__modulo__(3)
