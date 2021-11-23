---
layout: book
title: poppar
permalink: /learn/intrinsics/POPPAR
---
### NAME

**poppar**(3f) - \[BIT INQUIRY\] Parity of the number of bits set
(GFDL)

### SYNTAX

result = **poppar**(i)

### DESCRIPTION

Returns the parity of an integer's binary representation (i.e., the
parity of the number of bits set).

### ARGUMENTS

  - **I**
    Shall be of type INTEGER.

### RETURN VALUE

The return value is of type 'integer' and of the default integer kind.
It is equal to 0 if I has an even number of bits set and 1 if an odd
number of bits are set.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

**popcnt**(3), **leadz**(3), **trailz**(3)
