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
where the returned value is the same kind as the __KIND__, or of
the default kind if __KIND__ is not specified.

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
implicit none
character(len=40) :: string
character(len=:),allocatable :: astring
character(len=:),allocatable :: many_strings(:)
integer :: ii

   ii=len(string)
  write(*,*)'length =',ii

  ! the string length will be constant for the fixed-length variable
  string=' How long is this string? '
  write(*,'(a)')' ',string,repeat('=',len(string))

  ! the allocatable string length will be the length of LHS expression
  astring=' How long is this string? '
  write(*,'(a)')' ',astring,repeat('=',len(astring))
   
   ! you can also query the length (and other attributes) of a string
   ! using a "type parameter inquiry:" (available since fortran 2018)
   write(*,*)'length from type parameter inquiry=',string%len

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! define an allocatable array with a constructor ...
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)

   call proc_star(' how long? ')

contains

   subroutine proc_star(str)
   character(len=*),intent(in)  :: str
   character(len=:),allocatable :: str2
   ! the length of str can be used in the definitions of variables
   character(len=len(str))      :: str3

      if(allocated(str2))deallocate(str2)
      ! syntax for allocating a scalar string
      allocate(character(len=len(str)) :: str2)

      write(*,*)len(str),len(str2),len(str3)
      ! these are other allowable ways to define str2
      str2=str
      str2=repeat(' ',len(str))

   end subroutine proc_star

end program demo_len
```
Results:
```text
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
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (@urbanjost)
