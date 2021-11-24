---
layout: book
title: c_associated
permalink: /learn/intrinsics/C_ASSOCIATED
---
### NAME

__c\_associated__(3f) - \[ISO\_C\_BINDING\] Status of a C pointer
(GFDL)

### SYNTAX

result = __c\_associated__(c\_prt\_1\[, c\_ptr\_2\])

### DESCRIPTION

__c\_associated__(c\_prt\_1\[, c\_ptr\_2\]) determines the status of the
C pointer c\_ptr\_1 or if c\_ptr\_1 is associated with the target
c\_ptr\_2.

### ARGUMENTS

  - __c\_ptr\_1__
    Scalar of the type c\_ptr or c\_funptr.

  - __c\_ptr\_2__
    (Optional) Scalar of the same type as c\_ptr\_1.

### RETURN VALUE

The return value is of type LOGICAL; it is .false. if either c\_ptr\_1
is a C NULL pointer or if c\_ptr1 and c\_ptr\_2 point to different
addresses.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function

### SEE ALSO

__c\_loc__(3), __c\_funloc__(3), __iso\_c\_binding__(3)
