---
layout: book
title: digits
permalink: /learn/intrinsics/DIGITS
---
### NAME

**digits**(3f) - \[NUMERIC MODEL\] Significant digits function
(GFDL)

### SYNTAX

result = **digits**(x)

### DESCRIPTION

**digits**(x) returns the number of significant digits of the internal
model representation of X. For example, on a system using a 32-bit
floating point representation, a default real number would likely return
24.

### ARGUMENTS

  - **X**
    The type may be INTEGER or REAL.

### RETURN VALUE

The return value is of type INTEGER.

### EXAMPLE

Sample program:

```
    program demo_digits
    implicit none
        integer :: i = 12345
        real :: x = 3.143
        doubleprecision :: y = 2.33d0
        print *,'default integer:        ', digits(i)
        print *,'default real:           ', digits(x)
        print *,'default doubleprecision:', digits(y)
    end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function
