---
layout: book
title: repeat
permalink: /learn/intrinsics/REPEAT
---
#### NAME

__repeat__(3f) - \[CHARACTER\] Repeated string concatenation
(GFDL)

#### SYNTAX

result = __repeat__(string, ncopies)

#### DESCRIPTION

Concatenates NCOPIES copies of a string.

#### ARGUMENTS

  - __STRING__
    Shall be scalar and of type CHARACTER.

  - __NCOPIES__
    Shall be scalar and of type INTEGER.

#### RETURN VALUE

A new scalar of type CHARACTER built up from NCOPIES copies of STRING.

#### EXAMPLE

Sample program:

```
    program demo_repeat
    implicit none
      write(*,*) repeat("x", 5)   ! "xxxxx"
    end program demo_repeat
```

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function

#### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)
