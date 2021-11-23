---
layout: book
title: dprod
permalink: /learn/intrinsics/DPROD
---
### NAME

**dprod**(3f) - \[NUMERIC\] Double product function
(GFDL)

### SYNTAX

result = **dprod**(x, y)

### DESCRIPTION

**DPROD**(X,Y) produces a higher DOUBLEPRECISION product of default REAL
numbers X and Y.

The result has a value equal to a processor-dependent approximation to
the product of X and Y. It is recommended that the processor compute the
product in double precision, rather than in single precision and then
converted to double precision.

  - **X**
    shall be default real.

  - **Y**
    shall be default real.

The setting of compiler options specifying REAL size can affect this
function.

### ARGUMENTS

  - **X**
    Must be of default **REAL**(kind=kind(0.0)) type

  - **Y**
    Must have the same type and kind parameters as X

### RETURN VALUE

The return value is of type **real**(kind=kind(0.0d0)).

### EXAMPLE

Sample program:

```
    program demo_dprod
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    integer,parameter :: dp=kind(0.0d0)
    real :: x = 5.2
    real :: y = 2.3
    real(kind=dp) :: dd
       dd = dprod(x,y)
       print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
       ! interesting comparisons
       print *, 52*23
       print *, 52*23/100.0
       print *, 52*23/100.0d0

       !! common extension is to take doubleprecision arguments
       !! and return higher precision
       bigger: block
       doubleprecision :: xx = 5.2d0
       doubleprecision :: yy = 2.3d0
       real(kind=real128) :: ddd
       !ddd = dprod(xx,yy)
       !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
       endblock bigger

    end program demo_dprod
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function
