---
layout: book
title: c_f_pointer
permalink: /learn/intrinsics/C_F_POINTER
---
## __Name__

__c\_f\_pointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran pointer


## __Syntax__
```fortran
call c_f_pointer(cptr, fptr, shape)
```
## __Description__

__c\_f\_pointer(cptr, fptr\[, shape\])__ Assign the target, the C
pointer, __cptr__ to the Fortran pointer __fptr__ and specify its shape.

## __Arguments__

  - __cptr__
    : scalar of the type c\_ptr. It is __intent(in)__.

  - __fptr__
    : pointer interoperable with __cptr__. it is __intent(out)__.

  - __shape__
    : (Optional) Rank-one array of type _integer_ with __intent(in)__ .
    It shall be present if and only if __fptr__ is an array. The size
    must be equal to the rank of __fptr__.

## __Examples__

Sample program:

```fortran
program demo_c_f_pointer
use iso_c_binding
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
      import :: c_ptr
      type(c_ptr), intent(out) :: p
   end subroutine
end interface
type(c_ptr) :: cptr
real,pointer :: a(:)
   call my_routine(cptr)
   call c_f_pointer(cptr, a, [12])
end program demo_c_f_pointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
