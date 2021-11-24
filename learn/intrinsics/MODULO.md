---
layout: book
title: modulo
permalink: /learn/intrinsics/MODULO
---
### NAME

__modulo__(3f) - \[NUMERIC\] Modulo function
(GFDL)

### SYNTAX

result = __modulo__(a, p)

### DESCRIPTION

__modulo__(a,p) computes the A modulo P.

### ARGUMENTS

  - __A__
    Shall be a scalar of type INTEGER or REAL.

  - __P__
    Shall be a scalar of the same type and kind as A. It shall not be
    zero.

### RETURN VALUE

The type and kind of the result are those of the arguments.

  - If A and P are of type INTEGER: __modulo__(a,p) has the value of a -
    floor (__real__(a) / __real__(p)) \* p.

  - If A and P are of type REAL: __modulo__(a,p) has the value of a -
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

Elemental procedure\|Elemental function

### SEE ALSO

__mod__(3)
