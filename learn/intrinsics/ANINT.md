---
layout: book
title: anint
permalink: /learn/intrinsics/ANINT
---
## __Name__

__anint__(3) - \[NUMERIC\] Nearest whole number
(GFDL)

## __Syntax__

result = __anint__(a \[, kind\])

## __Description__

__anint__(a \[, kind\]) rounds its argument to the nearest whole number.

## __Arguments__

  - __A__
    : the type of the argument shall be _real_.

  - __KIND__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type real with the kind type parameter of the
argument if the optional __KIND__ is absent; otherwise, the kind type
parameter will be given by __KIND__. If __A__ is greater than zero, __anint__(a)
returns __aint(x + 0.5)__. If __A__ is less than or equal to zero then it
returns __aint(x - 0.5)__.

## __Examples__

Sample program:

```fortran
program demo_anint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8
   x4 = 1.234E0_real32
   x8 = 4.321_real64
   print *, anint(x4), dnint(x8)
   x8 = anint(x4,kind=real64)
end program demo_anint
```

## __Standard__

FORTRAN 77 and later
