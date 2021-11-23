---
layout: book
title: hypot
permalink: /learn/intrinsics/HYPOT
---
### NAME

**hypot**(3f) - \[MATHEMATICS\] Euclidean distance function
(GFDL)

### SYNTAX

result = **hypot**(x, y)

### DESCRIPTION

**hypot**(x,y) is the Euclidean distance function. It is equal to

```
      sqrt{X**2 + Y**2}, without undue underflow or overflow.
```

### ARGUMENTS

  - **X**
    The type shall be REAL.

  - **Y**
    The type and kind type parameter shall be the same as X.

### RETURN VALUE

The return value has the same type and kind type parameter as X.

### EXAMPLE

Sample program:

```
    program demo_hypot
    implicit none
      real(4) :: x = 1.e0_4, y = 0.5e0_4
      x = hypot(x,y)
    end program demo_hypot
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure\|Elemental function
