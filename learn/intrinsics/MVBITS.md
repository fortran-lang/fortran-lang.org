---
layout: book
title: mvbits
permalink: /learn/intrinsics/MVBITS
---
## __Name__

__mvbits__(3) - \[BIT:COPY\] Move bits from one integer to another


## __Syntax__
```fortran
call mvbits(from, frompos, len, to, topos)
```
## __Description__

__mvbits(3f)__ copies a range of adjacent bits from a specified position
in the _integer_ __from__ to a specified position in __to__ (which is
an integer of the same kind as __from__).  It otherwise leaves the bits
in __to__ as-is.

The bit positions copied must exist within the value of __from__.
That is, the values of __frompos+len-1__ and __topos+len-1__ must be
nonnegative and less than __bit\_size__(from).

The bits are numbered __0__ to __bit_size(i)-1__, from right to left.

## __Arguments__

  - __from__
    : An _integer_ to read bits from.
  - __frompos__
    : __frompos__ is the position of the first bit to copy. It is a
    nonnegative _integer_ value <= __bit_size(from)__.
  - __len__
    : A nonnegative _integer_ value that indicates how many bits to
    copy from __from__. It must not specify copying bits past the end
    of __from__. That is, __frompos + len__ must be less than or equal
    to __bit_size(from)__.
  - __to__
    : The _integer_ variable to place the copied bits into. It must
    be of the same kind as __from__ and may even be the same variable
    as __from__.

    __to__ 
    : is set by copying the sequence of bits of length __len__,
    starting at position __frompos__ of __from__ to position __topos__ of
    __to__. No other bits of __to__ are altered. On return, the __len__
    bits of __to__ starting at __topos__ are equal to the value that
    the __len__ bits of __from__ starting at __frompos__ had on entry.

  - __topos__
    : A nonnegative _integer_ value indicating the starting location in
    __to__ to place the specified copy of bits from __from__. 
    __topos + len__ must be less than or equal to __bit_size(to)__.

## __Example__
  Sample program that populates a new 32-bit integer with its bytes in
  reverse order (ie. changes the Endian of the integer).

      program demo_mvbits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: abcd_int
      character(len=*),parameter :: fmt= '(g0,t30,a,t40,b0)'
      
         ! make native integer value with bit patterns
         ! that happen to be the same as the beginning of the alphabet
	 ! to make it easy to see the bytes are reversed
         abcd_int=transfer('abcd',0)
         ! show the value and bit pattern
         write(*,*)'native'
         write(*,fmt)abcd_int,abcd_int,abcd_int
      
         ! change endian of the value
         abcd_int=int_swap32(abcd_int)
         ! show the values and their bit pattern
         write(*,*)'non-native'
         write(*,fmt)abcd_int,abcd_int,abcd_int
      
      contains
      
      pure elemental function int_swap32(intin) result(intout)
      ! Convert a 32 bit integer from big Endian to little Endian, 
      ! or conversely from little Endian to big Endian.
      !               
      integer(kind=int32), intent(in)  :: intin
      integer(kind=int32) :: intout
        ! copy bytes from input value to new position in output value
        !call mvbits(from,  frompos, len,   to,     topos)
         call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4
         call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3
         call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2
         call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1
      end function int_swap32
      
      end program demo_mvbits
```
  Results:
```text
    native
   1684234849                   abcd      1100100011000110110001001100001
    non-native
   1633837924                   dcba      1100001011000100110001101100100
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR)

###### fortran-lang intrinsic descriptions (License: MIT) @urbanjost
