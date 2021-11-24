---
layout: book
title: popcnt
permalink: /learn/intrinsics/POPCNT
---
### NAME

__popcnt__(3f) - \[BIT INQUIRY\] Number of bits set
(GFDL)

### SYNTAX

result = __popcnt__(i)

### DESCRIPTION

Returns the number of bits set in the binary representation of an
integer.

### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

### RETURN VALUE

The return value is of type 'integer' and of the default integer kind.

### EXAMPLE

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

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function
### SEE ALSO

__poppar__(3), __leadz__(3), __trailz__(3)
