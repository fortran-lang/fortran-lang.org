---
layout: book
title: scale
permalink: /learn/intrinsics/SCALE
---
### NAME

**scale**(3f) - \[MODEL\_COMPONENTS\] Scale a real value
(GFDL)

### SYNTAX

result = **scale**(x, i)

### DESCRIPTION

**scale**(x,i) returns x \* **radix**(x)\*\*i.

### ARGUMENTS

  - **X**
    The type of the argument shall be a REAL.

  - **I**
    The type of the argument shall be a INTEGER.

### RETURN VALUE

The return value is of the same type and kind as X. Its value is x \*
**radix**(x)\*\*i.

### EXAMPLE

Sample program:

```
    program demo_scale
    implicit none
      real :: x = 178.1387e-4
      integer :: i = 5
      print *, scale(x,i), x*radix(x)**i
    end program demo_scale
```

Results:

```
    0.570043862      0.570043862
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**radix**(3)
