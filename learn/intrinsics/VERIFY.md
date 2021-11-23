---
layout: book
title: verify
permalink: /learn/intrinsics/VERIFY
---
### NAME

**verify**(3f) - \[CHARACTER\] Scan a string for the absence of a set of characters
(GFDL)

### SYNTAX

result = **verify**(string, set\[, back \[, kind\]\])

### DESCRIPTION

Verifies that all the characters in STRING belong to the set of
characters in SET.

If BACK is either absent or equals FALSE, this function returns the
position of the leftmost character of STRING that is not in SET. If BACK
equals TRUE, the rightmost position is returned. If all characters of
STRING are found in SET, the result is zero.

### ARGUMENTS

  - **STRING**
    Shall be of type CHARACTER.

  - **SET**
    Shall be of type CHARACTER.

  - **BACK**
    (Optional) shall be of type LOGICAL.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### CLASS

Elemental function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
