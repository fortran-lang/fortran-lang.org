---
layout: book
title: minval
permalink: /learn/intrinsics/MINVAL
---
### NAME

**minval**(3f) - \[ARRAY REDUCTION\] Minimum value of an array
(GFDL)

### SYNTAX

result = **minval**(array, dim \[, mask\]) result = **minval**(array \[,
mask\])

### DESCRIPTION

Determines the minimum value of the elements in an array value, or, if
the DIM argument is supplied, determines the minimum value along each
row of the array in the DIM direction. If MASK is present, only the
elements for which MASK is .true. are considered. If the array has zero
size, or all of the elements of MASK are .false., then the result is
**huge**(array) if ARRAY is numeric, or a string of **char**(255)
characters if ARRAY is of character type.

### ARGUMENTS

  - **ARRAY**
    Shall be an array of type INTEGER, REAL, or CHARACTER.

  - **DIM**
    (Optional) Shall be a scalar of type INTEGER, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - **MASK**
    Shall be an array of type LOGICAL, and conformable with ARRAY.

### RETURN VALUE

If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.
If DIM is present, the result is an array with a rank one less than the
rank of ARRAY, and a size corresponding to the size of ARRAY with the
DIM dimension removed. In all cases, the result is of the same type and
kind as ARRAY.

### EXAMPLE

sample program:

```
    program demo_minval
    implicit none
    integer,save :: ints(3,5)= reshape([&
       1,  2,  3,  4,  5, &
      10, 20, 30, 40, 50, &
      11, 22, 33, 44, 55  &
    ],shape(ints),order=[2,1])
    write(*,*) minval(ints)
    write(*,*) minval(ints,dim=1)
    write(*,*) minval(ints,dim=2)
    end program demo_minval
```

results:

```
    > 1
    > 1    2     3     4     5
    > 1   10    11
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

**min**(3), **minloc**(3)
