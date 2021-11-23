---
layout: book
title: popcnt
permalink: /learn/intrinsics/POPCNT
---
### NAME

**popcnt**(3f) - \[BIT INQUIRY\] Number of bits set
(GFDL)

### SYNTAX

result = **popcnt**(i)

### DESCRIPTION

Returns the number of bits set in the binary representation of an
integer.

### ARGUMENTS

  - **I**
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

>   - **7**

  - **31**

      - **7**

  - **15**

      - **31**

      - **63**

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function 
### SEE ALSO

**poppar**(3), **leadz**(3), **trailz**(3)
