---
layout: book
title: minloc
permalink: /learn/intrinsics/f_minloc
---
### NAME

**minloc**(3f) - \[ARRAY LOCATION\] Location of the
minimum value within an array

### SYNTAX

result = **minloc**(array, dim \[, mask\]) result = **minloc**(array \[,
mask\])

### DESCRIPTION

Determines the location of the element in the array with the minimum
value, or, if the DIM argument is supplied, determines the locations of
the minimum element along each row of the array in the DIM direction. If
MASK is present, only the elements for which MASK is .true. are
considered. If more than one element in the array has the minimum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of MASK are
.false., then the result is an array of zeroes. Similarly, if DIM is
supplied and all of the elements of MASK along a given row are zero, the
result value for that row is zero.

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

If DIM is absent, the result is a rank-one array with a length equal to
the rank of ARRAY. If DIM is present, the result is an array with a rank
one less than the rank of ARRAY, and a size corresponding to the size of
ARRAY with the DIM dimension removed. If DIM is present and ARRAY has a
rank of one, the result is a scalar. In all cases, the result is of
default INTEGER type.

### EXAMPLE

sample program:

```
    program demo_minloc
    implicit none
    integer,save :: ints(3,5)= reshape([&
       4, 10,  1,  7, 13, &
       9, 15,  6, 12,  3, &
      14,  5, 11,  2,  8  &
    ],shape(ints),order=[2,1])
    write(*,*) minloc(ints)
    write(*,*) minloc(ints,dim=1)
    write(*,*) minloc(ints,dim=2)
    ! where in each column is the smallest number .gt. 10 ?
    write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
    ! a one-dimensional array with dim=1 explicitly listed returns a scalar
    write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
    end program demo_minloc
```

Results:

>   - **1**
>     3
>
>   - **1**
>     3 1 3 2
>
>   - **3**
>     5 4
>
>   - **5**
>     4 3
>
>   - **7**

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

**min**(3), **minval**(3)
