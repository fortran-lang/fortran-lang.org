---
layout: book
title: ichar
permalink: /learn/intrinsics/ICHAR
---
## __Name__

__ichar__(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function
(GFDL)

## __Syntax__
```fortran
   elemental function ichar(c,kind)

    character(len=1),intent(in) :: c
    integer,intent(in),optional :: kind
```
## __Description__

__ichar(c)__ returns the code for the character in the system's native
character set. The correspondence between characters and their codes is
not necessarily the same across different Fortran implementations. For
example, a platform using EBCDIC would return different values than an
ASCII platform.

See __iachar__(3) for specifically working with the ASCII character
set.

## __Arguments__

  - __c__
    : Shall be a scalar _character_, with __intent__(in)

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])
   do i=0,127
      call printme()
   enddo

contains

subroutine printme()
character(len=1) :: letter

   letter=char(i)
   select case(i)
   case (:31,127:)
      write(*,'(1x,i0.3,1x,"HEX=",z2.2,1x,i0)')i,letter,ichar(letter)
   case default
      write(*,'(1x,i0.3,1x,a,1x,i0)')i,letter,ichar(letter)
   end select

end subroutine printme

end program demo_ichar
```

## __Note__

No intrinsic exists to convert between a numeric value and a formatted
character string representation -- for instance, given the _character_
value '154', obtaining an _integer_ or _real_ value with the value 154, or
vice versa. Instead, this functionality is provided by internal-file
I/O, as in the following example:

```
program read_val
integer value
character(len=10) string, string2
   string = '154'

   ! Convert a string to a numeric value
   read (string,'(I10)') value
   print *, value

   ! Convert a value to a formatted string
   write (string2,'(I10)') value
   print *, string2
end program read_val
```

## __Standard__

Fortran 95 and later, with KIND argument -Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__iachar__(3)](IACHAR)

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
