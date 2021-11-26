---
layout: book
title: matmul
permalink: /learn/intrinsics/MATMUL
---
## __Name__

__matmul__(3) - \[TRANSFORMATIONAL FUNCTION\] matrix multiplication
(GFDL)

## __Syntax__

result = __matmul__(matrix\_a, matrix\_b)

## __Description__

Performs a matrix multiplication on numeric or logical arguments.

## __Arguments__

  - __matrix\_a__
    An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
    one or two.

  - __matrix\_b__
    An array of _integer_, _real_, or _complex_ type if matrix\_a is of a
    numeric type; otherwise, an array of _logical_ type. The rank shall be
    one or two, and the first (or only) dimension of matrix\_b shall be
    equal to the last (or only) dimension of matrix\_a.

## __Returns__

The matrix product of matrix\_a and matrix\_b. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

## __Standard__

Fortran 95 and later
