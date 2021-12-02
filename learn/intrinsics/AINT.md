---
layout: book
title: aint
permalink: /learn/intrinsics/AINT
---
## __Name__

__aint__(3) - \[NUMERIC\] Truncate to a whole number
(GFDL)

## __Syntax__

result = __aint__(A \[, KIND\])

## __Description__

__aint(A \[, KIND\])__ truncates its argument to a whole number.

## __Arguments__

  - __A__
    the type of the argument shall be _real_.

  - __KIND__
    (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _real_ with the kind type parameter of the
argument if the optional __KIND__ is absent; otherwise, the kind type
parameter will be given by __KIND__. If the magnitude of __X__ is less than one,
__aint(X)__ returns zero. If the magnitude is equal to or greater than
one then it returns the largest whole number that does not exceed its
magnitude. The sign is the same as the sign of __X__.

## __Examples__

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real ::  x4
real(kind=real64) :: x8
   x4 = 1.234E0_4
   x8 = 4.321_real64
   print *, aint(x4), dint(x8)
   x8 = aint(x4,kind=real64)
end program demo_aint
```

## __Standard__

FORTRAN 77 and later
