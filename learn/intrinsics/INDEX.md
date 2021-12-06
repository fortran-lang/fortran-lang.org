---
layout: book
title: index
permalink: /learn/intrinsics/INDEX
---
## __Name__

__index__(3) - \[CHARACTER:SEARCH\] Position of a substring within a string
(GFDL)

## __Syntax__

__index__(STRING, SUBSTRING \[, BACK \[, KIND\]\]) __result__(START)

```
     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: substring
     logical,intent(in),optional :: back
     integer,intent(in),optional :: kind
     integer(kind=KIND)          :: start
```

## __Description__

Returns the position of the start of the leftmost or rightmost
occurrence of string __substring__ in __string__, counting from one. If
__substring__ is not present in __string__, zero is returned.

## __Arguments__

  - __string__
    : string to be searched

  - __substring__
    : string to attempt to locate in __string__

  - __back__
    : If the __back__ argument is present and true, the return value is the
    start of the rightmost occurrence rather than the leftmost.

  - __kind__
    : An _integer_ initialization expression indicating the kind parameter
    of the result.

## __Returns__

  - __START__
    : The return value is of type _integer_ and of kind __kind__. If __kind__ is
    absent, the return value is of default integer kind.

## __Examples__

Example program

```fortran
program demo_index
implicit none
character(len=*),parameter :: str=&
   'Search this string for this expression'
   !1234567890123456789012345678901234567890
   write(*,*)&
      index(str,'this').eq.8,              &
      ! return value is counted from the left end even if BACK=.TRUE.
      index(str,'this',back=.true.).eq.24, &
      ! INDEX is case-sensitive
      index(str,'This').eq.0
end program demo_index
```
Expected Results:
```text
   T T T
```
## __Standard__

FORTRAN 77 and later, with KIND argument Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX), [__len\_trim__(3)](LEN_TRIM),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)
