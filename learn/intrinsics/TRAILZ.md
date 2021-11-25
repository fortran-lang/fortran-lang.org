---
layout: book
title: trailz
permalink: /learn/intrinsics/TRAILZ
---
### NAME

__trailz__(3f) - \[BIT INQUIRY\] Number of trailing zero bits of an integer

### SYNTAX
```fortran
   result = trailz(i) integer :: result
   integer(kind=NNN),intent(in) :: i
```

### DESCRIPTION

TRAILZ returns the number of trailing zero bits of an INTEGER value

### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

### RETURN VALUE

The type of the return value is the default INTEGER. If all the bits of
I are zero, the result value is __bit\_size__(I).

### EXAMPLE

Sample program:

```fortran
 program demo_trailz
 use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
 implicit none
 integer(kind=int64) :: i, value
   write(*,*)'Default integer:'
   write(*,*)'bit_size=',bit_size(0)
   write(*,'(1x,i3,1x,i3,1x,b0)')-1,trailz(1),-1
   write(*,'(1x,i3,1x,i3,1x,b0)')0,trailz(0),0
   write(*,'(1x,i3,1x,i3,1x,b0)')1,trailz(1),1
   write(*,'(" huge(0)=",i0,1x,i0,1x,b0)') &
   & huge(0),trailz(huge(0)),huge(0)
   write(*,*)
   write(*,*)'integer(kind=int64):'

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i19,1x,i3)')value,trailz(value)
   enddo
   value=huge(i)
   write(*,'(1x,i19,1x,i3,"(huge(0_int64))")')value,trailz(value)

   do i=-1,62,5
      value=2__i
      write(*,'(1x,i3,2x,b64.64)')i,value
   enddo
   value=huge(i)
   write(*,'(1x,a,1x,b64.64)') "huge",value

 end program demo_trailz
```

Results:

```
    Default integer:
    bit_size=          32
     -1   0 11111111111111111111111111111111
      0  32 0
      1   0 1
    huge(0)=2147483647 0 1111111111111111111111111111111

    integer(kind=int64):
                      0  64
                     16   4
                    512   9
                  16384  14
                 524288  19
               16777216  24
              536870912  29
            17179869184  34
           549755813888  39
         17592186044416  44
        562949953421312  49
      18014398509481984  54
     576460752303423488  59
    9223372036854775807   0(huge(0_int64))
     -1  0000000000000000000000000000000000000000000000000000000000000000
      4  0000000000000000000000000000000000000000000000000000000000010000
      9  0000000000000000000000000000000000000000000000000000001000000000
     14  0000000000000000000000000000000000000000000000000100000000000000
     19  0000000000000000000000000000000000000000000010000000000000000000
     24  0000000000000000000000000000000000000001000000000000000000000000
     29  0000000000000000000000000000000000100000000000000000000000000000
     34  0000000000000000000000000000010000000000000000000000000000000000
     39  0000000000000000000000001000000000000000000000000000000000000000
     44  0000000000000000000100000000000000000000000000000000000000000000
     49  0000000000000010000000000000000000000000000000000000000000000000
     54  0000000001000000000000000000000000000000000000000000000000000000
     59  0000100000000000000000000000000000000000000000000000000000000000
    huge 0111111111111111111111111111111111111111111111111111111111111111
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

__bit\_size__(3), __popcnt__(3), __poppar__(3), __leadz__(3)

###### fortran-lang intrinsic descriptions
