---
layout: book
title: ichar
permalink: /learn/intrinsics/ICHAR
---
### NAME

**ichar**(3f) - \[CHARACTER\] Character-to-integer conversion function
(GFDL)

### SYNTAX

elemental function **ichar**(c,kind)

```
    character(len=1),intent(in) :: c
    integer,intent(in),optional :: kind
```

### DESCRIPTION

**ICHAR**(C) returns the code for the character in the system's native
character set. The correspondence between characters and their codes is
not necessarily the same across different Fortran implementations. For
example, a platform using EBCDIC would return different values than an
ASCII platform.

See **IACHAR**(3f) for specifically working with the ASCII character
set.

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

```
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

### NOTE

No intrinsic exists to convert between a numeric value and a formatted
character string representation -- for instance, given the CHARACTER
value '154', obtaining an INTEGER or REAL value with the value 154, or
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

### STANDARD

Fortran 95 and later, with KIND argument -Fortran 2003 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

\[\[achar\]\], \[\[char\]\], \[\[iachar\]\]

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
