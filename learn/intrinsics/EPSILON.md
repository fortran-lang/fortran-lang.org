---
layout: book
title: epsilon
permalink: /learn/intrinsics/EPSILON
---
#### NAME

__epsilon__(3f) - \[NUMERIC MODEL\] Epsilon function
(GFDL)

#### SYNTAX

result = __epsilon__(x)

#### DESCRIPTION

__epsilon__(x) returns a nearly negligible number relative to 1.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of same type as the argument.

#### EXAMPLE

Sample program:

```
    program demo_epsilon
    implicit none
        real :: x = 3.143
        real(8) :: y = 2.33
        print *, epsilon(x)
        print *, epsilon(y)
    end program demo_epsilon
```

#### STANDARD

Fortran 95 and later

#### CLASS

Inquiry function
