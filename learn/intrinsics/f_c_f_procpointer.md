---
layout: book
title: c_f_procpointer
permalink: /learn/intrinsics/f_c_f_procpointer
---
### NAME

**c\_f\_procpointer**(3f) - \[ISO\_C\_BINDING\]
Convert C into Fortran procedure pointer

### SYNTAX

call **c\_f\_procpointer**(cptr, fptr)

### DESCRIPTION

**c\_f\_procpointer**(cptr, fptr) assigns the target of the C function
pointer CPTR to the Fortran procedure pointer FPTR.

### ARGUMENTS

  - **CPTR**
    scalar of the type c\_funptr. It is **intent**(in).

  - **FPTR**
    procedure pointer interoperable with CPTR. It is **intent**(out).

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

**c\_loc**(3), **c\_f\_pointer**(3), **iso\_c\_binding**(3)
