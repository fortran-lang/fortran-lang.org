---
layout: book
title: popcnt
permalink: /learn/intrinsics/POPCNT
---
## __Name__

__popcnt__(3) - \[BIT INQUIRY\] Number of bits set
(GFDL)

## __Syntax__

result = __popcnt__(i)

## __Description__

Returns the number of bits set in the binary representation of an
integer.

## __Arguments__

  - __I__
    Shall be of type _integer_.

## __Returns__

The return value is of type 'integer' and of the default integer kind.

## __Examples__

Sample program:

```
   program demo_popcnt
   use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
   implicit none
     print *, popcnt(127),       poppar(127)
     print *, popcnt(huge(0)), poppar(huge(0))
     print *, popcnt(huge(0_int8)), poppar(huge(0_int8))
     print *, popcnt(huge(0_int16)), poppar(huge(0_int16))
     print *, popcnt(huge(0_int32)), poppar(huge(0_int32))
     print *, popcnt(huge(0_int64)), poppar(huge(0_int64))
   end program demo_popcnt
```

Sample output:

>   - __7__

  - __31__

      - __7__

  - __15__

      - __31__

      - __63__

## __Standard__

Fortran 2008 and later

## __See Also__

__poppar__(3), __leadz__(3), __trailz__(3)
