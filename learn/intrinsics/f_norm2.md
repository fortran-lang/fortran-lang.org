---
layout: book
title: norm2
permalink: /learn/intrinsics/f_norm2
---
### NAME

**norm2**(3f) - \[MATHEMATICS\] Euclidean vector norm

### SYNTAX

result = **norm2**(array\[, dim\])

### DESCRIPTION

Calculates the Euclidean vector norm (L\_2 norm) of ARRAY along
dimension DIM.

### ARGUMENTS

  - **ARRAY**
    Shall be an array of type REAL.

  - **DIM**
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

### RETURN VALUE

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the square root of the sum of squares of
the elements of ARRAY is returned. Otherwise, an array of rank n-1,
where "n" equals the rank of ARRAY, and a shape similar to that of ARRAY
with dimension DIM dropped is returned.

### EXAMPLE

Sample program:

```
   program demo_norm2
   implicit none
     real :: x(5) = [ real :: 1, 2, 3, 4, 5 ]
     print *, norm2(x)  ! = sqrt(55.) ~ 7.416
   end program demo_norm2
```

### STANDARD

Fortran 2008 and later

### CLASS

Transformational function

### SEE ALSO

**product**(3), **sum**(3), **hypot**(3)
