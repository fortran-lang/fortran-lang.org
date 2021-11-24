---
layout: book
title: radix
permalink: /learn/intrinsics/RADIX
---
#### NAME

__radix__(3f) - \[NUMERIC MODEL\] Base of a model number
(GFDL)

#### SYNTAX

result = __radix__(x)

#### DESCRIPTION

__radix__(x) returns the base of the model representing the entity X.

#### ARGUMENTS

  - __X__
    Shall be of type INTEGER or REAL

#### RETURN VALUE

The return value is a scalar of type INTEGER and of the default integer
kind.

#### EXAMPLE

Sample program:

```
    program demo_radix
    implicit none
      print *, "The radix for the default integer kind is", radix(0)
      print *, "The radix for the default real kind is", radix(0.0)
    end program demo_radix
```

#### STANDARD

Fortran 95 and later

#### CLASS

Inquiry function

#### SEE ALSO

__scale__(3), __selected\_real\_kind__(3)
