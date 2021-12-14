---
layout: book
title: aimag
permalink: /learn/intrinsics/AIMAG
---
## __Name__

__aimag__(3) - \[TYPE:NUMERIC\] Imaginary part of complex number
(GFDL)

## __Syntax__
```fortran
    result = aimag(z)

     complex(kind=KIND),elemental :: aimag

     complex(kind=KIND),intent(in) :: z
```
## __Description__

__aimag(z)__ yields the imaginary part of complex argument __z__.

## __Arguments__

  - __z__
    : The type of the argument shall be _complex_.

## __Returns__

The return value is of type _real_ with the kind type parameter of the
argument.

## __Examples__

Sample program:

```fortran
program demo_aimag
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
complex(kind=real32) z4
complex(kind=real64) z8
    z4 = cmplx(1.e0, 2.e0)
    z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
    print *, aimag(z4), aimag(z8)
    ! an elemental function can be passed an array
    print *
    print *, [z4,z4/2.0,z4+z4,z4**3]
    print *
    print *, aimag([z4,z4/2.0,z4+z4,z4**3])
end program demo_aimag
```
Results:
```text
  2.000000       4.00000000000000

 (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)
 (-11.00000,-2.000000)

       2.000000       1.000000       4.000000      -2.000000
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
