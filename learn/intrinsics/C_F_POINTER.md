---
layout: book
title: c_f_pointer
permalink: /learn/intrinsics/C_F_POINTER
---
#### NAME

__c\_f\_pointer__(3f) - \[ISO\_C\_BINDING\] Convert C into Fortran pointer
(GFDL)

#### SYNTAX

call __c\_f\_pointer__(cptr, fptr\[, shape\])

#### DESCRIPTION

__c\_f\_pointer__(cptr, fptr\[, shape\]) Assign the target, the C
pointer, CPTR to the Fortran pointer FPTR and specify its shape.

#### ARGUMENTS

  - __CPTR__
    scalar of the type c\_ptr. It is __intent__(in).

  - __FPTR__
    pointer interoperable with CPTR. It is __intent__(out).

  - __SHAPE__
    (Optional) Rank-one array of type INTEGER with __intent__(in). It
    shall be present if and only if FPTR is an array. The size must be
    equal to the rank of FPTR.

#### EXAMPLE

Sample program:

```
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

#### STANDARD

Fortran 2003 and later

#### CLASS

Subroutine

#### SEE ALSO

__c\_loc__(3), __c\_f\_procpointer__(3), __iso\_c\_binding__(3)
