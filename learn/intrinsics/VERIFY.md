---
layout: book
title: verify
permalink: /learn/intrinsics/VERIFY
---
## __Name__

__verify__(3) - \[CHARACTER:SEARCH\] Scan a string for the absence of a set of characters
(GFDL)

## __Syntax__

result = __verify__(string, set\[, back \[, kind\]\])

## __Description__

Verifies that all the characters in __string__ belong to the set of
characters in __set__.

If __back__ is either absent or equals __.false.__, this function returns the
position of the leftmost character of __string__ that is not in __set__. If __back__
equals __.true.__, the rightmost position is returned. If all characters of
__string__ are found in __set__, the result is zero.

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
program demo_verify
implicit none
character(len=12):: c1='Howdy There!'
character(len=6) :: c2(2)=["Howdy ","there!"]
character(len=2) :: c3(2)=["de","gh"]
    
    !! location of first nonblank character
    write(*,*)'nonblank ',verify('  Hello World! ', ' ')

    !! same as len_trim(3)
    write(*,*)'length ',verify('  Hello World!    ', ' ', back = .true.)

    !! arrays
    write(*,*) verify(c1,'de')                   ! writes 1
    write(*,*) verify(c2,c3)                     ! writes 1 1
    write(*,*) verify(c1,'de',back=.true.)       ! writes 12
    write(*,*) verify(c2,c3,[.true.,.false.])    ! writes 6 1

    write(*,*) verify("fortran", "ao")           ! 1, found 'f'
    write(*,*) verify("fortran", "fo")           ! 3, found 'r'
    write(*,*) verify("fortran", "c++")          ! 1, found 'f'
    write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
    write(*,*) verify("fortran", "nartrof")      ! 0' found none


    !! CHECK IF STRING IS OF FORM NN-HHHHH
    CHECK : block
       logical                    :: lout
       character(len=*),parameter :: int='0123456789'
       character(len=*),parameter :: hex='abcdef0123456789'
       character(len=80)          :: chars
   
       chars='32-af43d'
       lout=.true.
       lout = lout.and.(verify(chars(1:2), int) == 0)
       lout = lout.and.(verify(chars(3:3), '-') == 0)
       lout = lout.and.(verify(chars(4:8), hex) == 0)
       if(lout)then
          write(*,*)trim(chars),' passed'
       endif
    endblock CHECK

   TEST_FUNCTIONS : block
   character(len=*),parameter :: ints(*)=[character(len=19) :: &
   '+1 ', &
   '3044848 ', &
   '30.40 ', &
   'September ', &
   '1 2 3', &
   '  -3000 ', &
   ' ']
   
   character(len=*),parameter :: symbols(*)=[character(len=19) :: &
   'A_ ', &
   '10 ', &
   ' ', &
   'September ', &
   'A B', &
   '_A ', &
   ' ']
   
   integer :: i
   
      write(*,'(4(g0,1x,g0,1x))')&
      & (ints(i),isint(ints(i)),i=1,size(ints))
      
      write(*,'(4(g0,1x,g0,1x))')&
      & (symbols(i),fortran_name(symbols(i)),i=1,size(symbols))

    endblock TEST_FUNCTIONS

contains

elemental function fortran_name(line) result (lout)
!
! determine if a string is a valid Fortran name 
! ignoring trailing spaces (but not leading spaces)
!
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      ! first character is alphameric
      lout = verify(name(1:1), lower//upper) == 0  &
       ! other characters are allowed in a symbol name
       & .and. verify(name,allowed) == 0           &
       ! allowable length
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name

elemental function isint(line) result (lout)
!
! determine if string is a valid integer representation 
! ignoring trailing spaces and leading spaces
!
character(len=*),parameter   :: digits='0123456789'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   lout=.false.
   name=adjustl(line)//'  ' ! make sure at least two characters long to simplify tests
   if( name .eq. '' )return                        ! blank string
   if( verify(name(1:1),'+-') == 0 ) name=name(2:) ! allow one leading sign
   if( name .eq. '' )return                        ! was just a sign
   lout=verify(trim(name), digits)  == 0  
end function isint

end program demo_verify
```
Results:
``` text
 nonblank            3
 length           14
           1
           1           1
          12
           6           1
           1
           3
           1
           7
           0
 32-af43d passed
+1                  T 3044848             T 30.40               F September           F
1 2 3               F   -3000             T                     F
A_                  T 10                  F                     F September           T
A B                 F _A                  F                     F
```

## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

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
