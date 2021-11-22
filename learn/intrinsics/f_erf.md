---
layout: book
title: erf
permalink: /learn/intrinsics/f_erf
---
### NAME

**erf**(3f) - \[MATHEMATICS\] Error function

### DESCRIPTION

**erf**(x) computes the error function of X, defined as $$
\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}} \\int\_0\*\*x
e\*\*{**-t**\*\*2} dt. $$

### SYNTAX

result = **erf**(x)

### ARGUMENTS

  - **X**
    The type shall be REAL.

### RETURN VALUE

The return value is of type REAL, of the same kind as X and lies in the
range **-1** \<= **erf**(x) \<= 1 .

### EXAMPLE

Sample program:

```
    program demo_erf
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    real(kind=real64) :: x = 0.17_real64
      x = erf(x)
    end program demo_erf
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure|Elemental function
