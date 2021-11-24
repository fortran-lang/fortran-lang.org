---
layout: book
title: erfc_scaled
permalink: /learn/intrinsics/ERFC_SCALED
---
### NAME

__erfc\_scaled__(3f) - \[MATHEMATICS\] Error function
(GFDL)

### DESCRIPTION

__erfc\_scaled__(x) computes the exponentially-scaled complementary
error function of X:

$$ e\*\*{x\*\*2} \\frac{2}{\\sqrt{\\pi}} \\int\_{x}\*\*{\\infty}
e\*\*{__-t__\*\*2} dt. $$

### SYNTAX

result = __erfc\_scaled__(x)

### ARGUMENTS

  - __X__
    The type shall be REAL.

### RETURN VALUE

The return value is of type REAL and of the same kind as X.

### EXAMPLE

Sample program:

```
   program demo_erfc_scaled
   implicit none
   real(kind(0.0d0)) :: x = 0.17d0
     x = erfc_scaled(x)
     print *, x ! prints approx. 0.83375830214998126
   end program demo_erfc_scaled
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function
