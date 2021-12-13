---
layout: book
title: scan
permalink: /learn/intrinsics/SCAN
---
## __Name__

__scan__(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters
(GFDL)

## __Syntax__
```fortran
result = scan(string, set[, back [, kind]])
```
## __Description__

Scans a __string__ for any of the characters in a __set__ of characters.

If __back__ is either absent or equals __.false.__, this function returns the
position of the leftmost character of __STRING__ that is in __set__. If __back__
equals __.true.__, the rightmost position is returned. If no character of __set__
is found in __string__, the result is zero.

## __Arguments__

  - __string__
    : Shall be of type _character_.

  - __set__
    : Shall be of type _character_.

  - __back__
    : (Optional) shall be of type _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
program demo_scan
implicit none
   write(*,*) scan("fortran", "ao")          ! 2, found 'o'
   write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
   write(*,*) scan("fortran", "c++")         ! 0, found none
end program demo_scan
```
  Results:
```text
              2
              6
              0
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)
