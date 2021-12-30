---
layout: book
title: maskr
permalink: /learn/intrinsics/MASKR
---
## __Name__

__maskr__(3) - \[BIT:SET\] Generates a right-justified mask

## __Syntax__
```fortran
result = maskr(i, kind)

  integer elemental function maskr(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskr(i\[, kind\])__ has its rightmost __i__ bits set to 1, and the
remaining bits set to 0.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it
specifies the kind value of the return type; otherwise, it is of the
default integer kind.

It has its rightmost __i__ bits set to 1, and the remaining bits set to 0.

##  __Example__

Sample program:
```fortrqn
program demo_maskr
implicit none
integer :: i
   i=maskr(1)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
   i=maskr(5)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
   i=maskr(11)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskr([(i,i,i=0,bit_size(0))])
end program demo_maskr
```
Results:
```text
1 1 10000000000000000000000000000000

31 11111 111000000000000000000000000000

2047 11111111111 111000000000000000000000

          0 0
          1 1
          3 11
          7 111
         15 1111
         31 11111
         63 111111
        127 1111111
        255 11111111
        511 111111111
       1023 1111111111
       2047 11111111111
       4095 111111111111
       8191 1111111111111
      16383 11111111111111
      32767 111111111111111
      65535 1111111111111111
     131071 11111111111111111
     262143 111111111111111111
     524287 1111111111111111111
    1048575 11111111111111111111
    2097151 111111111111111111111
    4194303 1111111111111111111111
    8388607 11111111111111111111111
   16777215 111111111111111111111111
   33554431 1111111111111111111111111
   67108863 11111111111111111111111111
  134217727 111111111111111111111111111
  268435455 1111111111111111111111111111
  536870911 11111111111111111111111111111
 1073741823 111111111111111111111111111111
 2147483647 1111111111111111111111111111111
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskl__(3)](MASKL)

###### fortran-lang intrinsic descriptions (license: MIT))
