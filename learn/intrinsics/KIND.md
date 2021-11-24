---
layout: book
title: kind
permalink: /learn/intrinsics/KIND
---
#### NAME

__kind__(3f) - \[KIND INQUIRY\] Kind of an entity
(GFDL)

#### SYNTAX

k = __kind__(x)

#### DESCRIPTION

__kind__(x) returns the kind value of the entity X.

#### ARGUMENTS

  - __X__
    Shall be of type LOGICAL, INTEGER, REAL, COMPLEX or CHARACTER.

#### RETURN VALUE

The return value is a scalar of type INTEGER and of the default integer
kind.

#### EXAMPLE

Sample program:

```
    program demo_kind
    implicit none
      integer,parameter :: kc = kind(' ')
      integer,parameter :: kl = kind(.true.)

    print *, "The default character kind is ", kc
      print *, "The default logical kind is ", kl
    end program demo_kind
```

#### STANDARD

Fortran 95 and later

#### CLASS

Inquiry function
