---
layout: book
title: len_trim
permalink: /learn/intrinsics/LEN_TRIM
---
### NAME

**len\_trim**(3f) - \[CHARACTER\] Length of a character entity without trailing blank characters

### SYNTAX

```fortran
   result = len_trim(string [, kind])
    character(len=*),intent(in) :: string
    integer,intent(in) :: kind
```

### DESCRIPTION

Returns the length of a character string, ignoring any trailing blanks.

### ARGUMENTS

  - **STRING**
    Shall be a scalar of type CHARACTER, with **intent**(in)

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

### EXAMPLE

Sample program

```fortran
     program demo_len_trim
     implicit none
     character(len=:),allocatable :: string
        string=' how long is this string?     '
        write(*,*)'LENGTH=',len(string)
        write(*,*)'TRIMMED LENGTH=',len_trim(string)
        !
        ELE:block ! elemental example
        character(len=:),allocatable :: tablet(:)
        tablet=[character(len=256) :: &
        & ' how long is this string?     ',&
        & 'and this one?']
           write(*,*)'LENGTH=',len(tablet)
           write(*,*)'TRIMMED LENGTH=',len_trim(tablet)
           write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
        endblock ELE
        !
     end program demo_len_trim
```

Results:

```
    LENGTH=          30
    TRIMMED LENGTH=          25
    LENGTH=         256
    TRIMMED LENGTH=          25          13
    SUM TRIMMED LENGTH=          38
```

### STANDARD

Fortran 95 and later, with KIND argument - Fortran 2003
and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)

#### @urbanjost
