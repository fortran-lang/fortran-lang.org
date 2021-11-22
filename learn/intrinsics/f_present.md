---
layout: book
title: present
permalink: /learn/intrinsics/f_present
---
### NAME

**present**(3f) - \[\] Determine whether an optional
dummy argument is specified

### SYNTAX

result = **present**(a)

### DESCRIPTION

Determines whether an optional dummy argument is present.

### ARGUMENTS

  - **A**
    May be of any type and may be a pointer, scalar or array value, or a
    dummy procedure. It shall be the name of an optional dummy argument
    accessible within the current subroutine or function.

### RETURN VALUE

Returns either TRUE if the optional argument A is present, or FALSE
otherwise.

### EXAMPLE

Sample program:

```
    program demo_present
    implicit none
      write(*,*) f(), f(42)      ! "f t"
    contains
      logical function f(x)
        integer, intent(in), optional :: x
        f = present(x)
      end function
    end program demo_present
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function
