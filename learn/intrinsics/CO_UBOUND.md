---
layout: book
title: co_ubound
permalink: /learn/intrinsics/CO_UBOUND
---
## __Name__

__co\_ubound__(3) - \[COLLECTIVE\] Upper codimension bounds of an array
(GFDL)

## __Syntax__

result = __CO\_UBOUND__(coarray \[, dim \[, kind\]\])

## __Description__

Returns the upper cobounds of a coarray, or a single upper cobound along
the DIM codimension.

## __Arguments__

  - __ARRAY__
    : Shall be an coarray, of any type.

  - __DIM__
    : (Optional) Shall be a scalar _integer_.

  - __KIND__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind. If DIM is absent, the
result is an array of the lower cobounds of COARRAY. If DIM is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

## __Standard__

Fortran 2008 and later

## __See Also__

[__co\_lbound__(3)](CO_LBOUND),
[__lbound__(3)](LBOUND),
[__ubound__(3)](UBOUND)
