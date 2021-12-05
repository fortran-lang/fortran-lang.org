---
layout: book
title: kind
permalink: /learn/intrinsics/KIND
---
## __Name__

__kind__(3) - \[KIND INQUIRY\] Kind of an entity
(GFDL)

## __Syntax__

k = __kind__(x)

## __Description__

__kind__(x) returns the kind value of the entity X.

## __Arguments__

  - __X__
    : Shall be of type _logical_, _integer_, _real_, _complex_ or CHARACTER.

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_kind
implicit none
integer,parameter :: kc = kind(' ')
integer,parameter :: kl = kind(.true.)

   print *, "The default character kind is ", kc
   print *, "The default logical kind is ", kl

end program demo_kind
```

## __Standard__

Fortran 95 and later
