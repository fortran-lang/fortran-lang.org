---
layout: book
title: verify
permalink: /learn/intrinsics/VERIFY
---
## __Name__

__verify__(3) - \[CHARACTER\] Scan a string for the absence of a set of characters
(GFDL)

## __Syntax__

result = __verify__(string, set\[, back \[, kind\]\])

## __Description__

Verifies that all the characters in STRING belong to the set of
characters in SET.

If BACK is either absent or equals FALSE, this function returns the
position of the leftmost character of STRING that is not in SET. If BACK
equals TRUE, the rightmost position is returned. If all characters of
STRING are found in SET, the result is zero.

## __Arguments__

  - __STRING__
    Shall be of type CHARACTER.

  - __SET__
    Shall be of type CHARACTER.

  - __BACK__
    (Optional) shall be of type _logical_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
    program demo_verify
    implicit none
    character(len=12):: c1='Howdy There!'
    character(len=6) :: c2(2)=["Howdy ","there!"]
    character(len=2) :: c3(2)=["de","gh"]
    !=======================================================
    !! LOCATION OF FIRST NONBLANK CHARACTER
    write(*,*)'nonblank ',verify('  Hello World! ', ' ')
    !! SAME AS LEN_TRIM()
    write(*,*)'length ',verify('  Hello World!    ', ' ', back = .true.)
    !! ARRAYS
    write(*,*) verify(c1,'de')                  ! writes 1
    write(*,*) verify(c2,c3)                    ! writes 1 1
    write(*,*) verify(c1,'de',back=.true.)      ! writes 12
    write(*,*) verify(c2,c3,[.true.,.false.]) ! writes 6 1
    !=======================================================
    write(*,*) verify("fortran", "ao")           ! 1, found 'f'
    write(*,*) verify("fortran", "fo")           ! 3, found 'r'
    write(*,*) verify("fortran", "c++")          ! 1, found 'f'
    write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
    write(*,*) verify("fortran", "nartrof")      ! 0' found none
    !=======================================================
    !! CHECK IF STRING IS OF FORM NN-HHHHH
    check : block
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

    endblock check
    end program demo_verify
```

Results:

```
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
```

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX), [__len\_trim__(3)](LEN_TRIM),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)
