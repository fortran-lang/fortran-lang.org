---
layout: book
title: same_type_as
permalink: /learn/intrinsics/SAME_TYPE_AS
---
# SAME_TYPE_AS
## __Name__

__same\_type\_as__(3) - \[STATE\] Query dynamic types for equality


## __Syntax__
```fortran
result = same_type_as(a, b)
```
## __Description__

Query dynamic types for equality.

## __Arguments__

  - __a__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

  - __b__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

## __Returns__

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of __a__ is the same as the dynamic type of __b__.

## __Standard__

Fortran 2003 and later

## __See Also__

[__extends\_type\_of__(3)](EXTENDS_TYPE_OF)

###### fortran-lang intrinsic descriptions
