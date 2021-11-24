---
layout: book
title: acos
permalink: /learn/intrinsics/ACOS
---
### NAME

__acos__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine function
(GFDL)

### SYNTAX

result = __ACOS__(X)

```
    real(kind=*),elemental :: x
```

### DESCRIPTION

__acos__(X) computes the arccosine of X (inverse of __cos__(x)).

### ARGUMENTS

  - __X__
    The type shall be REAL with a magnitude that is less than one.

### RETURN VALUE

The return value is of the same type and kind as X. The real part of the
result is in radians and lies in the range

> 0 \<= __ACOS__(X) \<= PI.

### EXAMPLE

Sample program:

```
   program demo_acos
   use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
   implicit none
   real(kind=real64) :: x = 0.866_real64
   real(kind=real64),parameter :: D2R=acos(-1.0_real64)/180.0_real64
     write(*,*)'acos(',x,') is ', acos(x)
     write(*,*)'90 degrees is ', d2r*90.0_real64, ' radians'
     write(*,*)'180 degrees is ', d2r*180.0_real64, ' radians'
     write(*,*)'for reference &
     &PI= 3.14159265358979323846264338327950288419716939937510'
    end program demo_acos
```

Results:

```
    acos(  0.86599999999999999      ) is   0.52364958093182890
    90 degrees is    1.5707963267948966       radians
    180 degrees is    3.1415926535897931       radians
    for reference PI= 3.14159265358979323846264338327950288419716939937510
```

### STANDARD

FORTRAN 77 and later; for a complex argument - Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

Inverse function: __cos__(3)
