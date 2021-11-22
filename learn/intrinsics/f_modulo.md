---
layout: book
title: modulo
permalink: /learn/intrinsics/f_modulo
---
### NAME

**modulo**(3f) - \[NUMERIC\] Modulo function

### SYNTAX

result = **modulo**(a, p)

### DESCRIPTION

**modulo**(a,p) computes the A modulo P.

### ARGUMENTS

  - **A**
    Shall be a scalar of type INTEGER or REAL.

  - **P**
    Shall be a scalar of the same type and kind as A. It shall not be
    zero.

### RETURN VALUE

The type and kind of the result are those of the arguments.

  - If A and P are of type INTEGER: **modulo**(a,p) has the value of a -
    floor (**real**(a) / **real**(p)) \* p.

  - If A and P are of type REAL: **modulo**(a,p) has the value of a -
    floor (a / p) \* p.

The returned value has the same sign as P and a magnitude less than the
magnitude of P.

### EXAMPLE

Sample program:

```
   program demo_modulo
   implicit none
     print *, modulo(17,3)        ! yields 2
     print *, modulo(17.5,5.5)    ! yields 1.0

     print *, modulo(-17,3)       ! yields 1
     print *, modulo(-17.5,5.5)   ! yields 4.5

     print *, modulo(17,-3)       ! yields -1
     print *, modulo(17.5,-5.5)   ! yields -4.5
   end program demo_modulo
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**mod**(3)
