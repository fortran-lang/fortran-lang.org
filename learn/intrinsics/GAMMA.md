---
layout: book
title: gamma
permalink: /learn/intrinsics/GAMMA
---
### NAME

**gamma**(3f) - \[MATHEMATICS\] Gamma function
(GFDL)

### DESCRIPTION

**gamma**(x) computes Gamma of X. For positive, integer values of X the
Gamma function simplifies to the factorial function
**Gamma**(x)=(x-1)\!.

$$ \\**Gamma**(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{**-t**}\\,{\\mathrm{d}}t $$

### SYNTAX

x = **gamma**(x)

### ARGUMENTS

  - **X**
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

Logarithm of the Gamma function: **\[\[log\_gamma**(3)
