---
layout: book
title: null
permalink: /learn/intrinsics/NULL
---
## __Name__

__null__(3) - \[TRANSFORMATIONAL FUNCTION\] Function that returns a disassociated pointer
(GFDL)

## __Syntax__

ptr =\> __null__(\[mold\])

## __Description__

Returns a disassociated pointer.

If MOLD is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In \[\[Fortran 95\]\], MOLD is optional. Please note that \[\[Fortran
2003\]\] includes cases where it is required.

## __Arguments__

  - __MOLD__
    (Optional) shall be a pointer of any association status and of any
    type.

## __Returns__

A disassociated pointer.

## __Examples__

Sample program:

```
    real, pointer, dimension(:) :: vec => null ()
```

## __Standard__

Fortran 95 and later

## __See Also__

__associated__(3)
