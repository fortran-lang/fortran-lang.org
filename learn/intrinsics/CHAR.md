---
layout: book
title: char
permalink: /learn/intrinsics/CHAR
---
## __Name__

__char__(3) - \[CHARACTER\] Character conversion function
(GFDL)

## __Syntax__

result = __char__(i \[, kind\])

## __Description__

__char__(i \[, kind\]) returns the character represented by the integer
I.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type __character__(1)

## __Examples__

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

## __Note__

See \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

FORTRAN 77 and later

## __See Also__

\[\[achar\]\], \[\[iachar\]\], \[\[ichar\]\]

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
