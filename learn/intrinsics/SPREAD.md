---
layout: book
title: spread
permalink: /learn/intrinsics/SPREAD
---
### NAME

__spread__(3f) - \[ARRAY CONSTRUCTION\] Add a dimension to an array
(GFDL)

### SYNTAX

result = __spread__(source, dim, ncopies)

### DESCRIPTION

Replicates a SOURCE array NCOPIES times along a specified dimension DIM.

### ARGUMENTS

  - __SOURCE__
    Shall be a scalar or an array of any type and a rank less than
    seven.

  - __DIM__
    Shall be a scalar of type INTEGER with a value in the range from 1
    to n+1, where n equals the rank of SOURCE.

  - __NCOPIES__
    Shall be a scalar of type INTEGER.

### RETURN VALUE

The result is an array of the same type as SOURCE and has rank n+1 where
n equals the rank of SOURCE.

### EXAMPLE

Sample program:

```
    program demo_spread
    implicit none
      integer :: a = 1, b(2) = [ 1, 2 ]
      write(*,*) spread(a, 1, 2)            ! "1 1"
      write(*,*) spread(b, 1, 2)            ! "1 1 2 2"
    end program demo_spread

     program example_spread
     !  Author:
     !    John Burkardt, 03 July 2006
     implicit none
     !
     integer ( kind = 4 ) a1(4,3)
     integer ( kind = 4 ) a2(3,4)
     integer i
     integer ( kind = 4 ) s
     integer ( kind = 4 ) v(4)
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'TEST_SPREAD'
     write ( *, '(a)' ) '  SPREAD is a FORTRAN90 function which replicates'
     write ( *, '(a)' ) '  an array by adding a dimension.'
     write ( *, '(a)' ) ' '
     !
     s = 99
     !
     write ( *, '(a,i6)' ) '  Suppose we have a scalar S = ', s
     write ( *, '(a)' ) ' '
     !
     v = spread ( s, 1, 4 )
     !
     write ( *, '(a)' ) '  V = spread ( s, 1, 4 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 4'
     write ( *, '(a)' ) ' '
     write ( *, '(4i6)' ) v(1:4)
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  Now first reset V to (1,2,3,4)'
     v = [ 1, 2, 3, 4 ]
     !
     a1 = spread ( v, 2, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A1 = spread ( v, 2, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (2) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 4
       write ( *, '(3i6)' ) a1(i,1:3)
     end do
     !
     a2 = spread ( v, 1, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A2 = spread ( v, 1, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 3
       write ( *, '(4i6)' ) a2(i,1:4)
     end do
     end program example_spread
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

__unpack__(3)
