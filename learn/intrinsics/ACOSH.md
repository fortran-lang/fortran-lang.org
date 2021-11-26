---
layout: book
title: acosh
permalink: /learn/intrinsics/ACOSH
---
### NAME

__acosh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic cosine function
(GFDL)

### SYNTAX

```
  result = acosh(x)
   TYPE(kind=KIND),elemental :: cosh
   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be REAL or COMPLEX and KIND may be any KIND supported
by the associated type.

### DESCRIPTION

__ACOSH__(X) computes the inverse hyperbolic cosine of X in radians.

### ARGUMENTS

  - __X__
    the type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has the same type and kind as X.

If X is complex, the imaginary part of the result is in radians and
lies between

> 0 \<= __AIMAG__(__ACOSH__(X)) \<= PI.

### EXAMPLE

Sample program:

```fortran
program demo_acosh
implicit none
real(8), dimension(3) :: x = [ 1.0, 2.0, 3.0 ]
   write (*,*) acosh(x)
end program demo_acosh
```

### STANDARD

Fortran 2008 and later

### SEE ALSO

Inverse function: __cosh__(3)
