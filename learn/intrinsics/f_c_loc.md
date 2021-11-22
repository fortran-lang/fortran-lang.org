---
layout: book
title: c_loc
permalink: /learn/intrinsics/f_c_loc
---
### NAME

**c\_loc**(3f) - \[ISO\_C\_BINDING\] Obtain the C
address of an object

### SYNTAX

result = **c\_loc**(x)

### DESCRIPTION

**c\_loc**(x) determines the C address of the argument.

### ARGUMENTS

  - **X**
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

**c\_associated**(3), **c\_funloc**(3), **c\_f\_pointer**(3),
**c\_f\_procpointer**(3), **iso\_c\_binding**(3)
