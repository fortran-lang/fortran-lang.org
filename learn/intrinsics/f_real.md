---
layout: book
title: real
permalink: /learn/intrinsics/f_real
---
### NAME

**real**(3f) - \[NUMERIC:TYPE\] Convert to real type

### SYNTAX

result = **real**(x \[, kind\])

### DESCRIPTION

**real**(x \[, kind\]) converts its argument X to a real type.

### ARGUMENTS

  - **X**
    Shall be INTEGER, REAL, or COMPLEX.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

These functions return a REAL variable or array under the following
rules:

1.  **real**(x) is converted to a default real type if X is an integer
    or real variable.

2.  **real**(x) is converted to a real type with the kind type parameter
    of X if X is a complex variable.

3.  **real**(x, kind) is converted to a real type with kind type
    parameter KIND if X is a complex, integer, or real variable.

### EXAMPLE

Sample program:

```
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

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

### SEE ALSO

**dble**(3), **float**(3)
