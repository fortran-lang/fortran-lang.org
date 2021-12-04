---
layout: book
title: modulo
permalink: /learn/intrinsics/MODULO
---
## __Name__

__modulo__(3) - \[NUMERIC\] Modulo function
(GFDL)

## __Syntax__
```fortran
result = modulo(a, p)

```
## __Description__

__modulo__(a,p) computes the __a__ modulo __p__.

## __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__. It shall not be
      zero.

## __Returns__

The type and kind of the result are those of the arguments.

  - If __a__ and __p__ are of type _integer_: __modulo(a,p)__ has the value of __a -
    floor (real(a) / real(p)) \* p__.

  - If __a__ and __p__ are of type _real_: __modulo(a,p)__ has the value of __a -
    floor (a / p) \* p__.

The returned value has the same sign as __p__ and a magnitude less than the
magnitude of __p__.

## __Examples__

Sample program:

```fortran
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
