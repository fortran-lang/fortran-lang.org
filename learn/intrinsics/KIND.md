---
layout: book
title: kind
permalink: /learn/intrinsics/KIND
---
## __Name__

__kind__(3) - \[KIND INQUIRY\] Kind of an entity


## __Syntax__
```fortran
k = kind(x)
```
## __Description__

__kind(x)__ returns the kind value of the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _logical_, _integer_, _real_, _complex_ or _character_.

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
  Results:
```text
    The default character kind is            1
    The default logical kind is            4
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
