---
layout: book
title: c_loc
permalink: /learn/intrinsics/C_LOC
---
### NAME

__c\_loc__(3f) - \[ISO\_C\_BINDING\] Obtain the C address of an object
(GFDL)

### SYNTAX

result = __c\_loc__(x)

### DESCRIPTION

__c\_loc__(x) determines the C address of the argument.

### ARGUMENTS

  - __X__
    Shall have either the POINTER or TARGET attribute. It shall not be a
    coindexed object. It shall either be a variable with interoperable
    type and kind type parameters, or be a scalar, nonpolymorphic
    variable with no length type parameters.

### RETURN VALUE

The return value is of type c\_ptr and contains the C address of the
argument.

### EXAMPLE

Sample program:

```
   subroutine association_test(a,b)
   use iso_c_binding, only: c_associated, c_loc, c_ptr
   implicit none
   real, pointer :: a
   type(c_ptr) :: b
     if(c_associated(b, c_loc(a))) &
        stop 'b and a do not point to same target'
   end subroutine association_test
```

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function

### SEE ALSO

__c\_associated__(3), __c\_funloc__(3), __c\_f\_pointer__(3),
__c\_f\_procpointer__(3), __iso\_c\_binding__(3)
