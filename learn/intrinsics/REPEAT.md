---
layout: book
title: repeat
permalink: /learn/intrinsics/REPEAT
---
## __Name__

__repeat__(3) - \[CHARACTER\] Repeated string concatenation
(GFDL)

## __Syntax__
```fortran
result = repeat(string, ncopies)

   character(len=len(string)*ncopies) :: repeat
   character(len=*),intent(in)        :: string
   integer,intent(in)                 :: ncopies
```
## __Description__

Concatenates __ncopies__ copies of a string.

## __Arguments__

  - __string__
    : The input string to repeatedly generate.
    Shall be scalar and of type _character_.

  - __ncopies__
    : Number of copies to make of _string_, greater than or equal to zero (0).
    Shall be scalar and of type _integer_.

## __Returns__

A new scalar of type _character_ built up from __ncopies__ copies of __string__.

## __Examples__

Sample program:

```fortran
program demo_repeat
implicit none
integer :: i, j
    write(*,'(a)') repeat("^v", 36)         ! line break
    write(*,'(a)') repeat("_", 72)          ! line break
    write(*,'(a)') repeat("1234567890", 7)  ! number line
    do i=80,0,-1 ! a simple progress bar
        write(*,'(a)',advance='no') &
        & repeat("#", i)//repeat(' ',80-i)//char(13)
        !do something slow
    enddo
end program demo_repeat
```
  Results:
```
   ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
   ________________________________________________________________________
   1234567890123456789012345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Non-elemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
