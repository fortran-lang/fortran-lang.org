---
layout: book
title: poppar
permalink: /learn/intrinsics/POPPAR
---
## __Name__

__poppar__(3) - \[BIT:COUNT\] Parity of the number of bits set
(GFDL)

## __Syntax__
```fortran
result = poppar(i)
```
## __Description__

Returns the parity of an integer's binary representation (i.e., the
parity of the number of bits set).

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is equal to __0__ if __i__ has an even number of bits set and 1 if an odd
number of bits are set.

It is of type _integer_ and of the default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
   print  *,  popcnt(127),            poppar(127)
   print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))
   print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))
   print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))
   print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))
end program demo_popcnt
```
  Results:
```text
              7           1
              7           1
             15           1
             31           1
             63           1
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__popcnt__(3)](POPCNT),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
