---
layout: book
title: char
permalink: /learn/intrinsics/f_char
---
### NAME

**char**(3f) - \[CHARACTER\] Character conversion
function

### SYNTAX

result = **char**(i \[, kind\])

### DESCRIPTION

**char**(i \[, kind\]) returns the character represented by the integer
I.

## ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type **character**(1)

### EXAMPLE

Sample program:

```
    program demo_char
    implicit none
    integer :: i = 74
    character(1) :: c
        c = char(i)
        print *, i, c ! returns 'J'
    end program demo_char
```

### NOTE

See \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

\[\[achar\]\], \[\[iachar\]\], \[\[ichar\]\]

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
