---
layout: book
title: len_trim
permalink: /learn/intrinsics/LEN_TRIM
---
## __Name__

__len\_trim__(3) - \[CHARACTER\] Length of a character entity without trailing blank characters

## __Syntax__

```fortran
   result = len_trim(string [, kind])
    character(len=*),intent(in) :: string
    integer,intent(in) :: kind
```

## __Description__

Returns the length of a character string, ignoring any trailing blanks.

## __Arguments__

  - __STRING__
    Shall be a scalar of type CHARACTER, with __intent__(in)

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind.

## __Examples__

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

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

##### fortran-lang intrinsic descriptions
