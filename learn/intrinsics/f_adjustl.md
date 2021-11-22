---
layout: book
title: adjustl
permalink: /learn/intrinsics/f_adjustl
---
### NAME

**adjustl**(3f) - \[CHARACTER\] Left-adjust a string

### SYNTAX


```fortran
    result = adjustl(string)
     character(len=*),intent(in) :: string
     character(len=len(string))  :: result
```

### DESCRIPTION

**adjustl**(STRING) will left adjust a string by removing leading
spaces. Spaces are inserted at the end of the string as needed.

### ARGUMENTS

  - **STRING**
    the type shall be CHARACTER.

### RETURN VALUE

The return value is of type CHARACTER and of the same kind as STRING
where leading spaces are removed and the same number of spaces are
inserted on the end of STRING.

### EXAMPLE

Sample program:

```fortran
    program demo_adjustl
    implicit none
    character(len=20) :: str = '   sample string'
    character(len=:),allocatable :: astr
       !
       ! basic use
       str = adjustl(str)
       write(*,'("[",a,"]")') str, trim(str)
       !
       ! an allocatable string stays the same length
       ! and is not trimmed.
       astr='    allocatable string   '
       write(*,'("[",a,"]")') adjustl(astr)
       !
    end program demo_adjustl
```

Results:

```
   [sample string       ]
   [sample string]
   [allocatable string       ]
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
