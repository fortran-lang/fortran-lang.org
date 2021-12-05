---
layout: book
title: adjustr
permalink: /learn/intrinsics/ADJUSTR
---
## __Name__

__adjustr__(3) - \[CHARACTER\] Right-adjust a string

## __Syntax__

```fortran
    result = adjustr(string)
     character(len=*),intent(in) :: string
     character(len=len(string))  :: result
```

## __Description__

__adjustr(string)__ will right-adjust a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed.

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
character(len=*),parameter :: bracket='("[",a,"]")'
character(len=20) :: str = ' sample string '
character(len=:),allocatable :: astr
   call number_line()
   !
   ! basic usage
   str = adjustr(str)
   write(*,bracket) str

   ! exploring usage:
   ! An allocatable string and arbitrary margin.
   ! Set a right margin and adjust to it. Note
   ! this would truncate if the margin is less
   ! than the length of STR
   right=50
   astr=adjustr(str//repeat(' ',max(0,right-len(str))))
   write(*,bracket) astr
   !
   call number_line()
   !
contains
   subroutine number_line()
   ! print a short number line
      write(*,bracket)repeat('1234567890',5)
   end subroutine number_line
end program demo_adjustr
```
Results:

```
   [12345678901234567890123456789012345678901234567890]
   [       sample string]
   [                                     sample string]
   [12345678901234567890123456789012345678901234567890]
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustl__(3)](ADJUSTL)

###### fortran-lang intrinsic descriptions (@urbanjost)
