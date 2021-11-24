---
layout: book
title: mod
permalink: /learn/intrinsics/MOD
---
#### NAME

__mod__(3f) - \[NUMERIC\] Remainder function
(GFDL)

#### SYNTAX

result = __mod__(a, p)

#### DESCRIPTION

__mod__(a,p) computes the remainder of the division of A by P.

#### ARGUMENTS

  - __A__
    Shall be a scalar of type INTEGER or REAL.

  - __P__
    Shall be a scalar of the same type and kind as A and not equal to
    zero.

#### RETURN VALUE

The return value is the result of a - (int(a/p) \* p). The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as A and a magnitude less than the magnitude of
P.

#### EXAMPLE

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

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__modulo__(3)
