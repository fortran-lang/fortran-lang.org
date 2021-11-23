---
layout: book
title: dot_product
permalink: /learn/intrinsics/DOT_PRODUCT
---
### NAME

**dot\_product**(3f) - \[TRANSFORMATIONAL FUNCTION\] Dot product function
(GFDL)

### SYNTAX

result = **dot\_product**(vector\_a, vector\_b)

### DESCRIPTION

**dot\_product**(vector\_a, vector\_b) computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are INTEGER or REAL, the result is
**sum**(vector\_a\*vector\_b). If the vectors are COMPLEX, the result is
**sum**(conjg(vector\_a)\*vector\_b). If the vectors are LOGICAL, the
result is **any**(vector\_a .and. vector\_b).

### ARGUMENTS

  - **vector\_a**
    The type shall be numeric or LOGICAL, rank 1.

  - **vector\_b**
    The type shall be numeric if vector\_a is of numeric type or LOGICAL
    if vector\_a is of type LOGICAL. vector\_b shall be a rank-one
    array.

### RETURN VALUE

If the arguments are numeric, the return value is a scalar of numeric
type, INTEGER, REAL, or COMPLEX. If the arguments are LOGICAL, the
return value is .true. or .false..

### EXAMPLE

Sample program:

```
    program demo_dot_prod
    implicit none
        integer, dimension(3) :: a, b
        a = [ 1, 2, 3 ]
        b = [ 4, 5, 6 ]
        print '(3i3)', a
        print *
        print '(3i3)', b
        print *
        print *, dot_product(a,b)
    end program demo_dot_prod
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function
