---
layout: book
title: logical
permalink: /learn/intrinsics/_logical_
---
## __Name__

__logical__(3) - \[BIT MANIPULATION\] Converts one kind of _logical_ variable to another
(GFDL)

## __Syntax__

result = __logical__(l \[, kind\])

## __Description__

Converts one kind of _logical_ variable to another.

## __Arguments__

  - __L__
    The type shall be _logical_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is a _logical_ value equal to L, with a kind
corresponding to KIND, or of the default logical kind if KIND is not
given.

## __Standard__

Fortran 95 and later

## __See Also__

__int__(3), __real__(3), __cmplx__(3)
