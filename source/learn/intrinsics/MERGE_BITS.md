---
layout: book
title: merge_bits
permalink: /learn/intrinsics/MERGE_BITS
---
## __Name__

__merge\_bits__(3) - \[BIT:COPY\] Merge bits using a mask

## __Syntax__
```fortran
result = merge_bits(i, j, mask)

    elemental function merge_bits(i,j,mask) result(r)
    integer(kind=KIND) ,intent(in) :: i, j, mask
    integer(kind=KIND) :: r
```
where the result and all input values have the same _integer_ type and
KIND with the exception that the mask and either __i__ or __j__ may be
a BOZ constant.

## __Description__

A common graphics operation in Ternary Raster Operations is to combine
bits from two different sources, generally referred to as bit-blending.
__merge\_bits__ performs a masked bit-blend of __i__ and __j__ using
the bits of the __mask__ value to determine which of the input values
to copy bits from.

Specifically, The k-th bit of the result is equal to the k-th bit of
__i__ if the k-th bit of __mask__ is __1__; it is equal to the k-th bit
of __j__ otherwise (so all three input values must have the same number
of bits).

The resulting value is the same as would result from

    __ior (iand (i, mask),iand (j, not (mask)))__

An exception to all values being of the same _integer_ type is that __i__
or __j__ and/or the mask may be a BOZ constant (A BOZ constant means it is
either a Binary, Octal, or Hexadecimal literal constant).  The BOZ values
are converted to the _integer_ type of the non-BOZ value(s) as if called
by the intrinsic function __int()__ with the kind of the non-BOZ value(s),
so the BOZ values must be in the range of the type of the result.

## __Arguments__

  - __i__ 
  : value to select bits from when the associated bit in the mask is __1__.

  - __j__
  : value to select bits from when the associated bit in the mask is __0__.

  - __mask__
  : a value whose bits are used as a mask to select bits from __i__ and __j__

## __Returns__

The bits blended from __i__ and __j__ using the mask __mask__. It is the
same type as __i__ if __i__ is of type _integer_, otherwise the same type
as __j__.

## __Example__
```fortran
program demo_merge_bits
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
implicit none
integer(kind=int16) :: if_one,if_zero,msk
character(len=*),parameter :: fmt='(*(g0, 1X))'

   ! basic usage
   print *,'MERGE_BITS( 5,10,41) should be 3.=>',merge_bits(5,10,41)
   print *,'MERGE_BITS(13,18,22) should be 4.=>',merge_bits(13,18,22)

   ! use some values in base2 illustratively:
   if_one =int(b'1010101010101010',kind=int16)
   if_zero=int(b'0101010101010101',kind=int16)

   msk=int(b'0101010101010101',kind=int16)
   print '("should get all zero bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk) 

   msk=int(b'1010101010101010',kind=int16)
   print '("should get all ones bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk) 

   ! using BOZ values
   print fmt, &
   & merge_bits(32767_int16,    o'12345',         32767_int16), &
   & merge_bits(o'12345'   , 32767_int16, b'0000000000010101'), &
   & merge_bits(32767_int16,    o'12345',             z'1234')

   ! a do-it-yourself equivalent for comparison and validation
   print fmt, &
   & ior(iand(32767_int16, 32767_int16),                   &
   &   iand(o'12345', not(32767_int16))),                  &

   & ior(iand(o'12345', int(o'12345', kind=int16)),        &
   &   iand(32767_int16, not(int(o'12345', kind=int16)))), &

   & ior(iand(32767_int16, z'1234'),                       &
   &   iand(o'12345', not(int( z'1234', kind=int16))))

end program demo_merge_bits
```
  Results:
```text
    MERGE_BITS( 5,10,41) should be 3.=>           3
    MERGE_BITS(13,18,22) should be 4.=>           4
   should get all zero bits =>0000000000000000
   should get all ones bits =>1111111111111111
   32767 32751 5877
   32767 32767 5877
```

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (license MIT) @urbanjost
