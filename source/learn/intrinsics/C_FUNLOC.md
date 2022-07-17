---
layout: book
title: c_funloc
permalink: /learn/intrinsics/C_FUNLOC
---
# C_FUNLOC
## __Name__

__c\_funloc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of a procedure


## __Syntax__
```fortran
result = c_funloc(x)
```
## __Description__

__c\_funloc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Interoperable function or pointer to such function.

## __Returns__

The return value is of type c\_funptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
! program demo_c_funloc and module
module x
use iso_c_binding
implicit none
contains
subroutine sub(a) bind(c)
real(c_float) :: a
   a = sqrt(a)+5.0
end subroutine sub
end module x
!
program demo_c_funloc
use iso_c_binding
use x
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
     import :: c_funptr
     type(c_funptr), intent(in) :: p
   end subroutine
end interface
   call my_routine(c_funloc(sub))
!
end program demo_c_funloc
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
