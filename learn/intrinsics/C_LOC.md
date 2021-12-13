---
layout: book
title: c_loc
permalink: /learn/intrinsics/C_LOC
---
## __Name__

__c\_loc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of an object
(GFDL)

## __Syntax__
```fortran
result = c_loc(x)
```
## __Description__

__c\_loc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Shall have either the _pointer_ or _target_ attribute. It shall not be a
    coindexed object. It shall either be a variable with interoperable
    type and kind type parameters, or be a scalar, nonpolymorphic
    variable with no length type parameters.

## __Returns__

The return value is of type c\_ptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
   subroutine association_test(a,b)
   use iso_c_binding, only: c_associated, c_loc, c_ptr
   implicit none
   real, pointer :: a
   type(c_ptr) :: b
     if(c_associated(b, c_loc(a))) &
        stop 'b and a do not point to same target'
   end subroutine association_test
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_funloc__(3)](C_FUNLOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
