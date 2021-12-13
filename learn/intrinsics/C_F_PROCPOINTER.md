---
layout: book
title: c_f_procpointer
permalink: /learn/intrinsics/C_F_PROCPOINTER
---
## __Name__

__c\_f\_procpointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran procedure pointer
(GFDL)

## __Syntax__
```fortran
call c_f_procpointer(cptr, fptr)
```
## __Description__

__c\_f\_procpointer(cptr, fptr)__ assigns the target of the C function
pointer __cptr__ to the Fortran procedure pointer __fptr__.

## __Arguments__

  - __cptr__
    : scalar of the type c\_funptr. It is __intent(in)__.

  - __fptr__
    : procedure pointer interoperable with __cptr__. It is __intent(out)__.

## __Examples__

Sample program:

```fortran
program demo_c_f_procpointer
use iso_c_binding
implicit none
abstract interface
   function func(a)
   import :: c_float
   real(c_float), intent(in) :: a
   real(c_float) :: func
   end function
end interface
interface
   function getIterFunc() bind(c,name="getIterFunc")
   import :: c_funptr
   type(c_funptr) :: getIterFunc
   end function
end interface
type(c_funptr) :: cfunptr
procedure(func), pointer :: myFunc
   cfunptr = getIterFunc()
   call c_f_procpointer(cfunptr, myFunc)
end program demo_c_f_procpointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
