---
layout: book
title: len
permalink: /learn/intrinsics/LEN
---
#### NAME

__len__(3f) - \[CHARACTER\] Length of a character entity
(GFDL)

#### SYNTAX

l = __len__(string \[, kind\])

#### DESCRIPTION

Returns the length of a character string. If STRING is an array, the
length of an element of STRING is returned. Note that STRING need not be
defined when this intrinsic is invoked, since only the length, not the
content, of STRING is needed.

#### ARGUMENTS

  - __STRING__
    Shall be a scalar or array of type CHARACTER, with __intent__(in)

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

#### STANDARD

FORTRAN 77 and later, with KIND argument - Fortran 2003 and later

#### CLASS

Inquiry function

#### EXAMPLE

Sample program

```
     program demo_len
     implicit none
     character(len=:),allocatable :: string
        string=' how long is this string?     '
        write(*,*)'LENGTH=',len(string)
        write(*,*)'TRIMMED LENGTH=',len_trim(string)
     end program demo_len
```

#### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
