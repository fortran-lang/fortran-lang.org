---
layout: book
title: len
permalink: /learn/intrinsics/LEN
---
## __Name__

__len__(3) - \[CHARACTER\] Length of a character entity

## __Syntax__
```fortran
   l = len(string, kind)

    integer(kind=KIND) function len(string,kind) result(value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
where the returned value is the same kind as the __KIND__  if it is 
specified.

## __Description__

__len(3)__ Returns the length of a _character_ string.

If __string__ is an array, the length of an element of __string__
is returned.

Note that __string__ need not be defined when this intrinsic is invoked,
as only the length (not the content) of __string__ is needed.

## __Arguments__

  - __string__
    : Shall be a scalar or array of type _character_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Standard__

FORTRAN 77 and later; with __kind__ argument - Fortran 2003 and later

## __Examples__

Sample program

```fortran
program demo_len
use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
implicit none
character(len=:),allocatable :: string
character(len=:),allocatable :: many_strings(:)
integer :: ii

   string=' How long is this string?     '
   ii=len(string)
   ! note when adjacent strings are printed no space is inserted between them
   write(*,*)'[',string,']',' length=',ii

   ! Related Matters:

   write(*,*)
   ! you can also query the length (and other attributes) of a string using a 
   write(*,*) "type parameter inquiry:"
   write(*,*)'length=',string%len,'kind=',string%kind
   ! note a type parameter inquiry of an intrinsic requires Fortran 2018+ 

   ! note that all that is required is an A descriptor in a format, a numeric
   ! length is not required. If a length IS provided the string will be trimmed
   ! or blank padded ON THE LEFT to the specified length
   write(*,*)
   write(*,'(" ",a," ")')repeat('=',ii)
   write(*,'("[",a,"]")')string
   write(*,'(" ",a," ")')repeat('=',ii)

   write(*,'("[",a10,"]")')string  ! TRUNCATED!
   write(*,'("[",a40,"]")')string  ! PADDED!
   ! you can specify the length at run time:
   ii=40
   write(*,'("[",a,"]")')[character(len=ii) :: string]  ! RIGHT JUSTIFIED!

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! stepping aside to define an allocatable array with a constructor ...
   ! (that MUST specify a LEN= length type parameter if all values are
   ! not the same length):
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   ! if the length specified is too short the strings will be truncated

   ! In that constructor, without the LEN= type specification, it would
   ! have been necessary to specify all of the constants with the same
   ! character length.

   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)
   write(*,'("[",a,"]")')many_strings

   ! Note in the following the result is always scalar, even if the
   ! object is an array.

   write(*,*) &
   & 'length=', many_strings%len, &
   & 'kind=', many_strings%kind
   ! which is the same as
   write(*,*) &
   & 'length=', len(many_strings), &
   & 'kind=', kind(many_strings)

   ! you should also be careful when printing strings that they do not
   ! exceed the current record length of your output file, although that
   ! usually is very large
   inquire(unit=stdout,recl=ii)
   write(*,*)'line length=',ii

end program demo_len
```
Results:
```text
 [ How long is this string?     ] length=          30
 
 type parameter inquiry:
 length=          30 kind=           1
 
 ============================== 
[ How long is this string?     ]
 ============================== 
[ How long ]
[           How long is this string?     ]
[ How long is this string?               ]
 
 length of ALL elements of array=           7
[Takata ]
[Tanaka ]
[Hayashi]
 length=           7 kind=           1
 length=           7 kind=           1
 line length=         132
```
## __See Also__

len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that
allow you to deal with leading and trailing blanks.

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
###### fortran-lang intrinsic descriptions (@urbanjost)
