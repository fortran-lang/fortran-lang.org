---
layout: book
title: repeat
permalink: /learn/intrinsics/REPEAT
---
### NAME

**repeat**(3f) - \[CHARACTER\] Repeated string concatenation
(GFDL)

### SYNTAX

result = **repeat**(string, ncopies)

### DESCRIPTION

Concatenates NCOPIES copies of a string.

### ARGUMENTS

  - **STRING**
    Shall be scalar and of type CHARACTER.

  - **NCOPIES**
    Shall be scalar and of type INTEGER.

### RETURN VALUE

A new scalar of type CHARACTER built up from NCOPIES copies of STRING.

### EXAMPLE

Sample program:

```
    program demo_repeat
    implicit none
      write(*,*) repeat("x", 5)   ! "xxxxx"
    end program demo_repeat
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
