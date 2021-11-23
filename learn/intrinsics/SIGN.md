---
layout: book
title: sign
permalink: /learn/intrinsics/SIGN
---
### NAME

**sign**(3f) - \[NUMERIC\] Sign copying function
(GFDL)

### SYNTAX

result = **sign**(a, b)

### DESCRIPTION

**sign**(a,b) returns the value of A with the sign of B.

### ARGUMENTS

  - **A**
    Shall be of type INTEGER or REAL

  - **B**
    Shall be of the same type and kind as A

### RETURN VALUE

The kind of the return value is that of A and B. If B \>= 0 then the
result is **abs**(a), else it is -**abs**(a).

### EXAMPLE

Sample program:

```
    program demo_sign
    implicit none
      print *, sign(-12,1)
      print *, sign(-12,0)
      print *, sign(-12,-1)

      print *, sign(-12.,1.)
      print *, sign(-12.,0.)
      print *, sign(-12.,-1.)
    end program demo_sign
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\\|Elemental function
