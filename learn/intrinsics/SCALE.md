---
layout: book
title: scale
permalink: /learn/intrinsics/SCALE
---
#### NAME

__scale__(3f) - \[MODEL\_COMPONENTS\] Scale a real value
(GFDL)

#### SYNTAX

result = __scale__(x, i)

#### DESCRIPTION

__scale__(x,i) returns x \* __radix__(x)\*\*i.

#### ARGUMENTS

  - __X__
    The type of the argument shall be a REAL.

  - __I__
    The type of the argument shall be a INTEGER.

#### RETURN VALUE

The return value is of the same type and kind as X. Its value is x \*
__radix__(x)\*\*i.

#### EXAMPLE

Sample program:

```
    program demo_scale
    implicit none
      real :: x = 178.1387e-4
      integer :: i = 5
      print *, scale(x,i), x*radix(x)**i
    end program demo_scale
```

Results:

```
    0.570043862      0.570043862
```

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__radix__(3)
