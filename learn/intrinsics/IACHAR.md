---
layout: book
title: iachar
permalink: /learn/intrinsics/IACHAR
---
### NAME

**iachar**(3f) - \[CHARACTER\] Code in ASCII collating sequence
(GFDL)

### SYNTAX

result = **iachar**(c \[, kind\])

### DESCRIPTION

**iachar**(c) returns the code for the ASCII character in the first
character position of C.

### ARGUMENTS

  - **C**
    Shall be a scalar CHARACTER, with **intent**(in)

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

### EXAMPLE

Sample program:

```fortran
   program demo_iachar
   implicit none
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

### NOTE

See \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

### STANDARD

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

\[\[achar\]\], \[\[char\]\], \[\[ichar\]\]

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
