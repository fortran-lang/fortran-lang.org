---
layout: book
title: real
permalink: /learn/intrinsics/_real_
---
## __Name__

__real__(3) - \[NUMERIC:TYPE\] Convert to real type
(GFDL)

## __Syntax__

result = __real__(x \[, kind\])

## __Description__

__real__(x \[, kind\]) converts its argument X to a real type.

## __Arguments__

  - __X__
    Shall be _integer_, _real_, or _complex_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

These functions return a _real_ variable or array under the following
rules:

1.  __real__(x) is converted to a default real type if X is an integer
    or real variable.

2.  __real__(x) is converted to a real type with the kind type parameter
    of X if X is a complex variable.

3.  __real__(x, kind) is converted to a real type with kind type
    parameter KIND if X is a complex, integer, or real variable.

## __Examples__

Sample program:

```fortran
   program demo_real
      use,intrinsic :: iso_fortran_env, only : dp=>real64
      implicit none
      complex              :: zr = (1.0, 2.0)
      doubleprecision      :: xd=huge(3.0d0)
      complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

      print *, real(zr), aimag(zr)
      print *, dble(zd), aimag(zd)

      write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
   end program demo_real
```

Results:

```
   1.00000000       2.00000000
   4.0000000000000000        5.0000000000000000
   1.7976931348623157E+308   1.7976931348623157E+308   1.7976931348623157E+308
```

## __Standard__

FORTRAN 77 and later

## __See Also__

__dble__(3), __float__(3)
