---
layout: book
title: c_f_procpointer
permalink: /learn/intrinsics/C_F_PROCPOINTER
---
### NAME

__c\_f\_procpointer__(3f) - \[ISO\_C\_BINDING\] Convert C into Fortran procedure pointer
(GFDL)

### SYNTAX

call __c\_f\_procpointer__(cptr, fptr)

### DESCRIPTION

__c\_f\_procpointer__(cptr, fptr) assigns the target of the C function
pointer CPTR to the Fortran procedure pointer FPTR.

### ARGUMENTS

  - __CPTR__
    scalar of the type c\_funptr. It is __intent__(in).

  - __FPTR__
    procedure pointer interoperable with CPTR. It is __intent__(out).

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2003 and later

### CLASS

Subroutine

### SEE ALSO

__c\_loc__(3), __c\_f\_pointer__(3), __iso\_c\_binding__(3)
