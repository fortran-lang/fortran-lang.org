---
layout: book
title: cos
permalink: /learn/intrinsics/COS
---
### NAME

**cos**(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function
(GFDL)

### SYNTAX

result = **cos**(x)

### DESCRIPTION

**cos**(x) computes the cosine of X.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value is of the same type and kind as X. The real part of the
result is in radians. If X is of the type REAL, the return value lies in
the range **-1** \<= **cos**(x) \<= 1.

### EXAMPLE

Sample program:

```
   program demo_cos
   implicit none
   doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
   end program demo_cos
```

Expected output:

```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**acos**(3), **sin**(3), **tan**(3)
