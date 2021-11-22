---
layout: book
title: c_funloc
permalink: /learn/intrinsics/f_c_funloc
---
### NAME

**c\_funloc**(3f) - \[ISO\_C\_BINDING\] Obtain the C
address of a procedure

### SYNTAX

result = **c\_funloc**(x)

### DESCRIPTION

**c\_funloc**(x) determines the C address of the argument.

### ARGUMENTS

  - **X**
    Interoperable function or pointer to such function.

### RETURN VALUE

The return value is of type c\_funptr and contains the C address of the
argument.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function

### SEE ALSO

**c\_associated**(3), **c\_loc**(3), **c\_f\_pointer**(3),
**c\_f\_procpointer**(3), **iso\_c\_binding**(3)
