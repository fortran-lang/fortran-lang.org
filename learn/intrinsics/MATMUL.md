---
layout: book
title: matmul
permalink: /learn/intrinsics/MATMUL
---
## __Name__

__matmul__(3) - \[TRANSFORMATIONAL\] matrix multiplication


## __Syntax__
```fortran
result = matmul(matrix_a, matrix_b)
```
## __Description__

Performs a matrix multiplication on numeric or logical arguments.

## __Arguments__

  - __matrix\_a__
    : An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
    one or two.

  - __matrix\_b__
    : An array of _integer_, _real_, or _complex_ type if __matrix\_a__ is of a
    numeric type; otherwise, an array of _logical_ type. The rank shall be
    one or two, and the first (or only) dimension of __matrix\_b__ shall be
    equal to the last (or only) dimension of __matrix\_a__.

## __Returns__

The matrix product of __matrix\_a__ and __matrix\_b__. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
