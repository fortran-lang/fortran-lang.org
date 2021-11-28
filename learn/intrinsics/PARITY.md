---
layout: book
title: parity
permalink: /learn/intrinsics/PARITY
---
## __Name__

__parity__(3) - \[TRANSFORMATIONAL FUNCTION\] Reduction with exclusive __OR__()
(GFDL)

## __Syntax__

result = __parity__(mask\[, dim\])

## __Description__

Calculates the parity (i.e. the reduction using .xor.) of MASK along
dimension DIM.

## __Arguments__

  - __MASK__
    Shall be an array of type _logical_.

  - __DIM__
    (Optional) shall be a scalar of type _integer_ with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

## __Returns__

The result is of the same type as MASK.

If DIM is absent, a scalar with the parity of all elements in MASK is
returned: .true. if an odd number of elements are .true. and

where "n" equals the rank of MASK, and a shape similar to that of MASK
with dimension DIM dropped is returned.

## __Examples__

Sample program:

```fortran
   program demo_parity
   implicit none
     logical :: x(2) = [ .true., .false. ]
     print *, parity(x) ! T
   end program demo_parity
```

## __Standard__

Fortran 2008 and later
