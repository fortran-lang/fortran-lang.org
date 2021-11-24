---
layout: book
title: tiny
permalink: /learn/intrinsics/TINY
---
#### NAME

__tiny__(3f) - \[NUMERIC MODEL\] Smallest positive number of a real kind
(GFDL)

#### SYNTAX

result = __tiny__(x)

#### DESCRIPTION

__tiny__(x) returns the smallest positive (non zero) number in the model
of the type of X.

#### ARGUMENTS

  - __X__
    Shall be of type REAL.

#### RETURN VALUE

The return value is of the same type and kind as X

#### EXAMPLE

Sample program:

```
    program demo_huge_tiny
    implicit none
      print *, huge(0), huge(0.0), huge(0.0d0)
      print *, tiny(0.0), tiny(0.0d0)
    end program demo_huge_tiny
```

#### STANDARD

Fortran 95 and later

#### CLASS

Inquiry function
