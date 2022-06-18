---
layout: book
title: iachar
permalink: /learn/intrinsics/IACHAR
---
## __Name__

__iachar__(3) - \[CHARACTER:CONVERSION\] Code in ASCII collating sequence
```fortran
   elemental function iachar(c,KIND) result(i)
   integer(kind=KIND) :: i
   character(len=1),intent(in) :: c
   integer,intent(in),optional :: KIND
```
The return value is of type _integer_ and of kind __KIND__. If __KIND__ is absent,
the return value is of default integer kind.

## __Syntax__
```fortran
result = iachar(c, kind)
```
## __Description__

__iachar__(c) returns the code for the ASCII character in the first
character position of __c__.

   If __c__ is an ASCII character (ie. in the collating sequence defined
   by ISO 646:1983) the result is the position of __c__ in that sequence
   and is in the range 0 to 127.

   An undefined value is returned if __c__ is not in the ASCII collating
   sequence.

## __Arguments__

  - __c__
    : The character determines the value of the ADE
    (ASCII Decimal Equivalent) that will be returned.

  - __kind__
    :  An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__
   The _integer_ code for the first character in __c__. It represents
   the position of the character in the ASCII collating sequence.

## __Examples__

Sample program:

```fortran
program demo_iachar
implicit none

write(*,*)iachar(['a','z','A','Z'])

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
  Results:
```text
             97         122          65          90
   abcdefg abcdefg
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

###### fortran-lang intrinsic descriptions (license MIT) @urbanjost
