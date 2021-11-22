---
layout: book
title: log_gamma
permalink: /learn/intrinsics/f_log_gamma
---
### NAME

**log\_gamma**(3f) - \[MATHEMATICS\] Logarithm of the
Gamma function

### SYNTAX

x = **log\_gamma**(x)

### DESCRIPTION

**log\_gamma**(x) computes the natural logarithm of the absolute value
of the \[\[Gamma function\]\].

### ARGUMENTS

  - **X**
    Shall be of type REAL and neither zero nor a negative integer.

### RETURN VALUE

The return value is of type REAL of the same kind as X.

### EXAMPLE

Sample program:

```
   program demo_log_gamma
   implicit none
     real :: x = 1.0
     x = log_gamma(x) ! returns 0.0
   end program demo_log_gamma
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

Gamma function: **gamma**(3)
