---
layout: book
title: log
permalink: /learn/intrinsics/LOG
---
### NAME

__log__(3f) - \[MATHEMATICS\] Logarithm function
(GFDL)

### SYNTAX

result = __LOG__(X)

### DESCRIPTION

__LOG__(X) computes the natural logarithm of X, i.e. the logarithm to
the base "e".

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value is of type REAL or COMPLEX. The kind type parameter is
the same as X. If X is COMPLEX, the imaginary part OMEGA is in the range

__-PI__ \< OMEGA \<= PI.

### EXAMPLE

Sample program:

```
   program demo_log
   implicit none
     real(kind(0.0d0)) :: x = 2.71828182845904518d0
     complex :: z = (1.0, 2.0)
     x = log(x)    ! will yield (approximately) 1
     z = log(z)
   end program demo_log
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function
