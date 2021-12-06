---
layout: book
title: len
permalink: /learn/intrinsics/LEN
---
## __Name__

__len__(3) - \[CHARACTER\] Length of a character entity
(GFDL)

## __Syntax__
```fortran
   l = len(string, kind)

    integer(kind=KIND) elemental function len(string,kind) result(value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: kind
    integer :: value
```

## __Description__

Returns the length of a character string. If __string__ is an array, the
length of an element of __string__ is returned. Note that __string__ need not be
defined when this intrinsic is invoked, since only the length, not the
content, of __string__ is needed.

## __Arguments__

  - __string__
    : Shall be a scalar or array of type _character_, with __intent__(in)

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Standard__

FORTRAN 77 and later, with __kind__ argument - Fortran 2003 and later

## __Examples__

Sample program

```fortran
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
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__len\_trim__(3)](LEN_TRIM),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)
