---
layout: book
title: iachar
permalink: /learn/intrinsics/IACHAR
---
### NAME

__iachar__(3f) - \[CHARACTER\] Code in ASCII collating sequence
(GFDL)

### SYNTAX

result = __iachar__(c \[, kind\])

### DESCRIPTION

__iachar__(c) returns the code for the ASCII character in the first
character position of C.

### ARGUMENTS

  - __C__
    Shall be a scalar CHARACTER, with __intent__(in)

  - __KIND__
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

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
