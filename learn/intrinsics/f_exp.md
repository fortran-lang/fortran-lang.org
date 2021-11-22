---
layout: book
title: exp
permalink: /learn/intrinsics/f_exp
---
### NAME

**exp**(3f) - \[MATHEMATICS\] Exponential function

### SYNTAX

result = **exp**(x)

### DESCRIPTION

**exp**(x) computes the base "e" exponential of X.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has same type and kind as X.

### EXAMPLE

Sample program:

```
    program demo_exp
    implicit none
      real :: x = 1.0
      x = exp(x)
    end program demo_exp
```

### STANDARD

FORTRAN 77 and later

### CLASS

\[\[Elemental procedure|Elemental function\]\]
