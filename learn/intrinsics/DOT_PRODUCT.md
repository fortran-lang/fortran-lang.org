---
layout: book
title: dot_product
permalink: /learn/intrinsics/DOT_PRODUCT
---
#### NAME

__dot\_product__(3f) - \[TRANSFORMATIONAL FUNCTION\] Dot product function
(GFDL)

#### SYNTAX

result = __dot\_product__(vector\_a, vector\_b)

#### DESCRIPTION

__dot\_product__(vector\_a, vector\_b) computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are INTEGER or REAL, the result is
__sum__(vector\_a\*vector\_b). If the vectors are COMPLEX, the result is
__sum__(conjg(vector\_a)\*vector\_b). If the vectors are LOGICAL, the
result is __any__(vector\_a .and. vector\_b).

#### ARGUMENTS

  - __vector\_a__
    The type shall be numeric or LOGICAL, rank 1.

  - __vector\_b__
    The type shall be numeric if vector\_a is of numeric type or LOGICAL
    if vector\_a is of type LOGICAL. vector\_b shall be a rank-one
    array.

#### RETURN VALUE

If the arguments are numeric, the return value is a scalar of numeric
type, INTEGER, REAL, or COMPLEX. If the arguments are LOGICAL, the
return value is .true. or .false..

#### EXAMPLE

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

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function
