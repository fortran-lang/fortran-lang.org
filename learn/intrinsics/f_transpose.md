---
layout: book
title: transpose
permalink: /learn/intrinsics/f_transpose
---
### NAME

**transpose**(3f) - \[ARRAY MANIPULATION\] Transpose
an array of rank two

### SYNTAX

result = **transpose**(matrix)

### DESCRIPTION

Transpose an array of rank two. Element (i, j) of the result has the
value **matrix**(j, i), for all i, j.

### ARGUMENTS

  - **MATRIX**
    Shall be an array of any type and have a rank of two.

### RETURN VALUE

The result has the same type as MATRIX, and has shape \[ m, n \] if
MATRIX has shape \[ n, m \].

### EXAMPLE

Sample program:

```
    program demo_transpose
    implicit none
    integer,save :: xx(3,5)= reshape([&
        1,  2,  3,  4,  5,    &
       10, 20, 30, 40, 50,    &
       11, 22, 33, 44, -1055  &
     ],shape(xx),order=[2,1])
    call print_matrix_int('xx array:',xx)
    call print_matrix_int('xx array transposed:',transpose(xx))
    contains
    subroutine print_matrix_int(title,arr)
    ! print small 2d integer arrays in row-column format
    implicit none
    character(len=*),intent(in)  :: title
    integer,intent(in)           :: arr(:,:)
    integer                      :: i
    character(len=:),allocatable :: biggest
       write(*,*)trim(title)  ! print title
       biggest='           '  ! make buffer to write integer into
       ! find how many characters to use for integers
       write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
       ! use this format to write a row
       biggest='(" > [",*(i'//trim(biggest)//':,","))'
       ! print one row of array at a time
       do i=1,size(arr,dim=1)
          write(*,fmt=biggest,advance='no')arr(i,:)
          write(*,'(" ]")')
       enddo
    end subroutine print_matrix_int
    end program demo_transpose
```

Results:

```
    xx array:
    > [     1,     2,     3,     4,     5 ]
    > [    10,    20,    30,    40,    50 ]
    > [    11,    22,    33,    44, -1055 ]
    xx array transposed:
    > [     1,    10,    11 ]
    > [     2,    20,    22 ]
    > [     3,    30,    33 ]
    > [     4,    40,    44 ]
    > [     5,    50, -1055 ]
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function
