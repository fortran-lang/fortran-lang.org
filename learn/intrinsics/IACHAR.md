---
layout: book
title: iachar
permalink: /learn/intrinsics/IACHAR
---
## __Name__

__iachar__(3) - \[CHARACTER:CONVERSION\] Code in ASCII collating sequence
(GFDL)

## __Syntax__

result = __iachar__(c \[, kind\])

## __Description__

__iachar__(c) returns the code for the ASCII character in the first
character position of C.

## __Arguments__

  - __c__
    : Shall be a scalar _character_, with _intent(in)_

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
program demo_iachar
implicit none
! create function to convert uppercase letters to lowercase
   write(*,'(a)')lower('abcdefg ABCDEFG')
contains
!
elemental pure function lower(str) result (string)
! Changes a string to lowercase
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   ! step thru each letter in the string in specified range
   do i = 1, len(str)
      select case (str(i:i))
      case ('A':'Z') ! change letter to miniscule
         string(i:i) = char(iachar(str(i:i))+32)
      case default
      end select
   end do
end function lower
!
end program demo_iachar
```

## __Note__

See [__ichar__(3)](ICHAR) for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__ichar__(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX), 
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)
