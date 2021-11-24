---
layout: book
title: char
permalink: /learn/intrinsics/CHAR
---
#### NAME

__char__(3f) - \[CHARACTER\] Character conversion function
(GFDL)

#### SYNTAX

result = __char__(i \[, kind\])

#### DESCRIPTION

__char__(i \[, kind\]) returns the character represented by the integer
I.

#### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is of type __character__(1)

#### EXAMPLE

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

#### NOTE

See \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

\[\[achar\]\], \[\[iachar\]\], \[\[ichar\]\]

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
