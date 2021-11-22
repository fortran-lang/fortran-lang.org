---
layout: book
title: spacing
permalink: /learn/intrinsics/f_spacing
---
### NAME

**spacing**(3f) - \[MODEL\_COMPONENTS\] Smallest
distance between two numbers of a given type

### SYNTAX

result = **spacing**(x)

### DESCRIPTION

Determines the distance between the argument X and the nearest adjacent
number of the same type.

### ARGUMENTS

  - **X**
    Shall be of type REAL.

### RETURN VALUE

The result is of the same type as the input argument X.

### EXAMPLE

Sample program:

```
    program demo_spacing
    implicit none
      integer, parameter :: sgl = selected_real_kind(p=6, r=37)
      integer, parameter :: dbl = selected_real_kind(p=13, r=200)

      write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
      write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
    end program demo_spacing
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental function

### SEE ALSO

**rrspacing**(3)
