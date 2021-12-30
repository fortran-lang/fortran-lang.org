---
layout: book
title: co_lbound
permalink: /learn/intrinsics/CO_LBOUND
---
## __Name__

__co\_lbound__(3) - \[COLLECTIVE\] Lower codimension bounds of an array


## __Syntax__
```fortran
result = co_lbound(coarray, dim, kind)
```
## __Description__

Returns the lower bounds of a coarray, or a single lower cobound along
the __dim__ codimension.

## __Arguments__

  - __array__
    : Shall be an coarray, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower cobounds of __coarray__. If __dim__ is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_ubound__(3)](CO_UBOUND),
[__lbound__(3)](LBOUND)

###### fortran-lang intrinsic descriptions
