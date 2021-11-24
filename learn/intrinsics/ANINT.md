---
layout: book
title: anint
permalink: /learn/intrinsics/ANINT
---
#### NAME

__anint__(3f) - \[NUMERIC\] Nearest whole number
(GFDL)

#### SYNTAX

result = __anint__(a \[, kind\])

#### DESCRIPTION

__anint__(a \[, kind\]) rounds its argument to the nearest whole number.

#### ARGUMENTS

  - __A__
    the type of the argument shall be REAL.

  - __KIND__
    (optional) an INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is of type real with the kind type parameter of the
argument if the optional KIND is absent; otherwise, the kind type
parameter will be given by KIND. If A is greater than zero, __anint__(a)
returns __aint__(x + 0.5). If A is less than or equal to zero then it
returns __aint__(x - 0.5).

#### EXAMPLE

Sample program:

```
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

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function
