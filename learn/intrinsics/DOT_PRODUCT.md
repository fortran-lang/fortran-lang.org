---
layout: book
title: dot_product
permalink: /learn/intrinsics/DOT_PRODUCT
---
## __Name__

__dot\_product__(3) - \[TRANSFORMATIONAL FUNCTION\] Dot product function
(GFDL)

## __Syntax__

result = __dot\_product__(vector\_a, vector\_b)

## __Description__

__dot\_product__(vector\_a, vector\_b) computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are _integer_ or _real_, the result is
__sum__(vector\_a\*vector\_b). If the vectors are _complex_, the result is
__sum__(conjg(vector\_a)\*vector\_b). If the vectors are _logical_, the
result is __any__(vector\_a .and. vector\_b).

## __Arguments__

  - __vector\_a__
    The type shall be numeric or _logical_, rank 1.

  - __vector\_b__
    The type shall be numeric if vector\_a is of numeric type or _logical_
    if vector\_a is of type _logical_. vector\_b shall be a rank-one
    array.

## __Returns__

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is .true. or .false..

## __Examples__

Sample program:

```
    program demo_dot_prod
    implicit none
        integer, dimension(3) :: a, b
        a = [ 1, 2, 3 ]
        b = [ 4, 5, 6 ]
        print '(3i3)', a
        print *
        print '(3i3)', b
        print *
        print *, dot_product(a,b)
    end program demo_dot_prod
```

## __Standard__

Fortran 95 and later
