---
layout: book
title: adjustr
permalink: /learn/intrinsics/f_adjustr
---
### NAME

**adjustr**(3f) - \[CHARACTER\] Right-adjust a string

### SYNTAX

```fortran
    result = adjustr*(string)
     character(len=*),intent(in) :: string
     character(len=len(string))  :: result
```

### DESCRIPTION

**adjustr**(STRING) will right adjust a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed.

### ARGUMENTS

  - **STRING**
    the type shall be CHARACTER.

### RETURN VALUE

The return value is of type CHARACTER and of the same kind as STRING
where trailing spaces are removed and the same number of spaces are
inserted at the start of STRING.

### EXAMPLE

Sample program:

```fortran
    program demo_adjustr
    implicit none
    integer :: right
    character(len=*),parameter :: bracket='("[",a,"]")'
    character(len=20) :: str = ' sample string '
    character(len=:),allocatable :: astr
       call number_line()
       !
       ! basic usage
       str = adjustr(str)
       write(*,bracket) str

       ! exploring usage:
       ! An allocatable string and arbitrary margin.
       ! Set a right margin and adjust to it. Note
       ! this would truncate if the margin is less
       ! than the length of STR
       right=50
       astr=adjustr(str//repeat(' ',max(0,right-len(str))))
       write(*,bracket) astr
       !
       call number_line()
       !
    contains
       subroutine number_line()
       ! print a short number line
          write(*,bracket)repeat('1234567890',5)
       end subroutine number_line
    end program demo_adjustr
```

Results:

```
   [12345678901234567890123456789012345678901234567890]
   [       sample string]
   [                                     sample string]
   [12345678901234567890123456789012345678901234567890]
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **nonelemental:**
    **repeat**(3), **trim**(3)

#### @urbanjost
