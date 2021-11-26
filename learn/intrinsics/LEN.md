---
layout: book
title: len
permalink: /learn/intrinsics/LEN
---
## __Name__

__len__(3) - \[CHARACTER\] Length of a character entity
(GFDL)

## __Syntax__

l = __len__(string \[, kind\])

## __Description__

Returns the length of a character string. If STRING is an array, the
length of an element of STRING is returned. Note that STRING need not be
defined when this intrinsic is invoked, since only the length, not the
content, of STRING is needed.

## __Arguments__

  - __STRING__
    Shall be a scalar or array of type CHARACTER, with __intent__(in)

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind.

## __Standard__

FORTRAN 77 and later, with KIND argument - Fortran 2003 and later

## __Examples__

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

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
