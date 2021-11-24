---
layout: book
title: sign
permalink: /learn/intrinsics/SIGN
---
#### NAME

__sign__(3f) - \[NUMERIC\] Sign copying function
(GFDL)

#### SYNTAX

result = __sign__(a, b)

#### DESCRIPTION

__sign__(a,b) returns the value of A with the sign of B.

#### ARGUMENTS

  - __A__
    Shall be of type INTEGER or REAL

  - __B__
    Shall be of the same type and kind as A

#### RETURN VALUE

The kind of the return value is that of A and B. If B \>= 0 then the
result is __abs__(a), else it is -__abs__(a).

#### EXAMPLE

Sample program:

```
    program demo_sign
    implicit none
      print *, sign(-12,1)
      print *, sign(-12,0)
      print *, sign(-12,-1)

      print *, sign(-12.,1.)
      print *, sign(-12.,0.)
      print *, sign(-12.,-1.)
    end program demo_sign
```

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\\|Elemental function
