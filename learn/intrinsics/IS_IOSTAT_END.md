---
layout: book
title: is_iostat_end
permalink: /learn/intrinsics/IS_IOSTAT_END
---
### NAME

**is\_iostat\_end**(3f) - \[\] Test for end-of-file value
(GFDL)

### SYNTAX

function **is\_iostat\_end**(i)

```
    logical            :: is_iostat_end (i)
    integer,intent(in) :: i
```

### DESCRIPTION

is\_iostat\_end tests whether an variable has the value of the I/O
status "end of file". The function is equivalent to comparing the
variable with the iostat\_end parameter of the intrinsic module
\[\[iso\_fortran\_env\]\].

### ARGUMENTS

  - **I**
    Shall be of the type INTEGER.

### RETURN VALUE

Returns a LOGICAL of the default kind, which .true. if I has the value
which indicates an end of file condition for IOSTAT= specifiers, and is
.false. otherwise.

### EXAMPLE

Sample program:

```
    program demo_iostat
      implicit none
      integer :: stat, i
      open(88, file='test.dat')
      read(88, *, iostat=stat) i
      if(is_iostat_end(stat)) stop 'end of file'
    end program demo_iostat
```

### STANDARD

Fortran 2003 and later

### CLASS

Elemental function
