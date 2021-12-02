---
layout: book
title: nul l
permalink: /learn/intrinsics/NULL
---
## __Name__

__null__(3) - \[TRANSFORMATIONAL FUNCTION\] Function that returns a disassociated pointer
(GFDL)

## __Syntax__
```fortran
ptr => null(mold)

```

## __Description__

Returns a disassociated pointer.

If __mold__ is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In _Fortran 95_, __mold__ is optional. Please note that _Fortran 2003_ includes cases where it is required.

## __Arguments__

  - __mold__
    (Optional) shall be a pointer of any association status and of any
    type.

## __Returns__

A disassociated pointer.

## __Examples__

Sample program:

```fortran
program demo_null
real, pointer, dimension(:) :: vec => null ()
end program demo_null
```

## __Standard__

Fortran 95 and later

## __See Also__

__associated__(3)
