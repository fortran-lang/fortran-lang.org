---
layout: book
title: gamma
permalink: /learn/intrinsics/GAMMA
---
### NAME

__gamma__(3f) - \[MATHEMATICS\] Gamma function
(GFDL)

### DESCRIPTION

__gamma__(x) computes Gamma of X. For positive, integer values of X the
Gamma function simplifies to the factorial function
__Gamma__(x)=(x-1)\!.

$$ \\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t $$

### SYNTAX

x = __gamma__(x)

### ARGUMENTS

  - __X__
    Shall be of type REAL and neither zero nor a negative integer.

### RETURN VALUE

The return value is of type REAL of the same kind as X.

### EXAMPLE

Sample program:

```
   program demo_gamma
   implicit none
     real :: x = 1.0
     x = gamma(x) ! returns 1.0
   end program demo_gamma
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

Logarithm of the Gamma function: __\[\[log\_gamma__(3)
