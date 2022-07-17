---
layout: book
title: c_associated
permalink: /learn/intrinsics/C_ASSOCIATED
---
# C_ASSOCIATED
## __Name__

__c\_associated__(3) - \[ISO\_C\_BINDING\] Status of a C pointer


## __Syntax__
```fortran
result = c_associated(c_prt_1, c_ptr_2)
```
## __Description__

__c\_associated(c\_prt\_1\[, c\_ptr\_2\])__ determines the status of the
C pointer c\_ptr\_1 or if c\_ptr\_1 is associated with the target
c\_ptr\_2.

## __Arguments__

  - __c\_ptr\_1__
    : Scalar of the type c\_ptr or c\_funptr.

  - __c\_ptr\_2__
    : (Optional) Scalar of the same type as c\_ptr\_1.

## __Returns__

The return value is of type _logical_; it is .false. if either c\_ptr\_1
is a C NULL pointer or if c\_ptr1 and c\_ptr\_2 point to different
addresses.

## __Examples__

Sample program:

```fortran
program demo_c_associated

contains

subroutine association_test(a,b)
use iso_c_binding, only: c_associated, c_loc, c_ptr
implicit none
real, pointer :: a
type(c_ptr) :: b
   if(c_associated(b, c_loc(a))) &
      stop 'b and a do not point to same target'
end subroutine association_test

end program demo_c_associated
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_funloc__(3)](C_FUNLOC),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
