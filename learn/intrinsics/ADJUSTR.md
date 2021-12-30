---
layout: book
title: adjustr
permalink: /learn/intrinsics/ADJUSTR
---
## __Name__

__adjustr__(3) - \[CHARACTER:WHITESPACE\] Right-adjust a string

## __Syntax__
```fortran
    result = adjustr(string)

     elemental function adjustr(string)
     character(len=*),intent(in) :: string
     character(len=(len(string)) :: adjustr
```
## __Description__

__adjustr(string)__ right-adjusts a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed to
retain the original length.

## __Arguments__

  - __string__
    : the type shall be _character_.

## __Returns__

The return value is of type _character_ and of the same kind as __string__
where trailing spaces are removed and the same number of spaces are
inserted at the start of __string__.

## __Examples__

Sample program:

```fortran
program demo_adjustr
implicit none
integer :: right
character(len=20) :: str = ' sample string '
character(len=:),allocatable :: str2
   ! print a short number line
   write(*,'(a)')repeat('1234567890',5)

   !
   ! basic usage
   !
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])
    
   write(*,'(a)')repeat('1234567890',5)
end program demo_adjustr
```
Results:
```text
   12345678901234567890123456789012345678901234567890
          sample string
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustl__(3)](ADJUSTL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
