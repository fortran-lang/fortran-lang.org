---
layout: book
title: maxloc
permalink: /learn/intrinsics/MAXLOC
---
### NAME

**maxloc**(3f) - \[ARRAY LOCATION\] Location of the maximum value within an array
(GFDL)

### SYNTAX

result = **maxloc**(array, dim \[, mask\]) result = **maxloc**(array \[,
mask\])

### DESCRIPTION

Determines the location of the element in the array with the maximum
value, or, if the DIM argument is supplied, determines the locations of
the maximum element along each row of the array in the DIM direction. If
MASK is present, only the elements for which MASK is .true. are
considered. If more than one element in the array has the maximum value,
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

The value returned is reference to the offset from the beginning of the
array, not neccessarily the subscript value if the array subscripts do
not start with one.

### EXAMPLE

sample program:

```
    program demo_maxloc
    implicit none
    integer      :: ii
    integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
    integer,save :: ints(3,5)= reshape([&
       1,  2,  3,  4,  5, &
      10, 20, 30, 40, 50, &
      11, 22, 33, 44, 55  &
    ],shape(ints),order=[2,1])
    write(*,*) maxloc(ints)
    write(*,*) maxloc(ints,dim=1)
    write(*,*) maxloc(ints,dim=2)
    ! when array bounds do not start with one remember MAXLOC(3f) returns the
    ! offset relative to the lower bound-1 of the location of the maximum
    ! value, not the subscript of the maximum value. When the lower bound of
    ! the array is one, these values are the same. In other words, MAXLOC(3f)
    ! returns the subscript of the value assuming the first subscript of the
    ! array is one no matter what the lower bound of the subscript actually
    ! is.
    write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
    write(*,*)maxloc(i)

    end program demo_maxloc
```

expected output:

>   - **3**
>
>   - **3**
>     3 3 3 3
>
>   - **5**
>     5 5 **-3** 47 **-2** 48 **-1** 49 0 50 1 49 2 48 3 47 4

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

**max**(3), **maxval**(3)
