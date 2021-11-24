---
layout: book
title: hypot
permalink: /learn/intrinsics/HYPOT
---
### NAME

__hypot__(3f) - \[MATHEMATICS\] Euclidean distance function
(GFDL)

### SYNTAX

result = __hypot__(x, y)

### DESCRIPTION

__hypot__(x,y) is the Euclidean distance function. It is equal to

```
      sqrt{X__2 + Y__2}, without undue underflow or overflow.
```

### ARGUMENTS

  - __X__
    The type shall be REAL.

  - __Y__
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
