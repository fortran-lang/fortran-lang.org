---
layout: book
title: maxexponent
permalink: /learn/intrinsics/MAXEXPONENT
---
#### NAME

__maxexponent__(3f) - \[NUMERIC MODEL\] Maximum exponent of a real kind
(GFDL)

#### SYNTAX

result = __maxexponent__(x)

#### DESCRIPTION

__maxexponent__(x) returns the maximum exponent in the model of the type
of X.

#### ARGUMENTS

  - __X__
    Shall be of type REAL.

#### RETURN VALUE

The return value is of type INTEGER and of the default integer kind.

#### EXAMPLE

Sample program:

```
    program demo_maxexponent
    implicit none
      real(kind=4) :: x
      real(kind=8) :: y

      print *, minexponent(x), maxexponent(x)
      print *, minexponent(y), maxexponent(y)
    end program demo_maxexponent
```

#### STANDARD

Fortran 95 and later

#### CLASS

Inquiry function
