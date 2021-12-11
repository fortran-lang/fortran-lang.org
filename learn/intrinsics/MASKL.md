---
layout: book
title: maskl
permalink: /learn/intrinsics/MASKL
---
## __Name__

__maskl__(3) - \[BIT:MANIPULATION\] Generates a left justified mask
## __Syntax__
```fortran
result = maskl(i, kind)

  integer elemental function maskl(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskl(i\[, *kind*\])__ has its leftmost __i__ bits set to __1__, and the
remaining bits set to __0__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

The leftmost __i__ bits are set to 1 and the other bits are set to 0.

## Example
Sample program:
```fortran
program demo_maskl
implicit none
integer :: i
   i=maskl(1)
   write(*,'(i0,1x,b0,/)') i,i,shiftl(7,bit_size(0)-1
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskl([(i,i,i=1,bit_size(0))])
end program demo_maskl
```
  Results:
```text
-2147483648 10000000000000000000000000000000

          0 0
-2147483648 10000000000000000000000000000000
-1073741824 11000000000000000000000000000000
 -536870912 11100000000000000000000000000000
 -268435456 11110000000000000000000000000000
 -134217728 11111000000000000000000000000000
  -67108864 11111100000000000000000000000000
  -33554432 11111110000000000000000000000000
  -16777216 11111111000000000000000000000000
   -8388608 11111111100000000000000000000000
   -4194304 11111111110000000000000000000000
   -2097152 11111111111000000000000000000000
   -1048576 11111111111100000000000000000000
    -524288 11111111111110000000000000000000
    -262144 11111111111111000000000000000000
    -131072 11111111111111100000000000000000
     -65536 11111111111111110000000000000000
     -32768 11111111111111111000000000000000
     -16384 11111111111111111100000000000000
      -8192 11111111111111111110000000000000
      -4096 11111111111111111111000000000000
      -2048 11111111111111111111100000000000
      -1024 11111111111111111111110000000000
       -512 11111111111111111111111000000000
       -256 11111111111111111111111100000000
       -128 11111111111111111111111110000000
        -64 11111111111111111111111111000000
        -32 11111111111111111111111111100000
        -16 11111111111111111111111111110000
         -8 11111111111111111111111111111000
         -4 11111111111111111111111111111100
         -2 11111111111111111111111111111110
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskr__(3)](MASKR)

###### fortran-lang intrinsic descriptions (@urbanjost)
