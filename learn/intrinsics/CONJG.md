---
layout: book
title: conjg
permalink: /learn/intrinsics/CONJG
---
## __Name__

__conjg__(3) - \[NUMERIC\] Complex conjugate function
(GFDL)

## __Syntax__

z = __conjg__(z)

## __Description__

__conjg__(z) returns the conjugate of Z. If Z is (x, y) then the result
is (x, __-y__)

## __Arguments__

  - __Z__
    The type shall be _complex_.

## __Returns__

The return value is of type _complex_.

## __Examples__

Sample program:

```fortran
    program demo_conjg
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    complex :: z = (2.0, 3.0)
    complex(kind=real64) :: dz = (&
    &  1.2345678901234567_real64, &
    & -1.2345678901234567_real64)
        z= conjg(z)
        print *, z
        dz = conjg(dz)
        print *, dz
    end program demo_conjg
```

## __Standard__

FORTRAN 77 and later
