## popcnt
### __Name__

__popcnt__(3) - \[BIT:COUNT\] Number of bits set


### __Syntax__
```fortran
result = popcnt(i)
```
### __Description__

Returns the number of bits set in the binary representation of an
_integer_.

### __Arguments__

  - __i__
    : Shall be of type _integer_.

### __Returns__

The return value is of type _integer_ and of the default integer kind.

### __Examples__

Sample program:

```fortran
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
Results:
```text
        7           1
       31           1
        7           1
       15           1
       31           1
       63           1
```
### __Standard__

Fortran 2008 and later

### __See Also__

[__poppar__(3)](POPPAR),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

####### fortran-lang intrinsic descriptions
