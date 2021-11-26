---
layout: book
title: same_type_as
permalink: /learn/intrinsics/SAME_TYPE_AS
---
## __Name__

__same\_type\_as__(3) - \[\] Query dynamic types for equality
(GFDL)

## __Syntax__

result = __same\_type\_as__(a, b)

## __Description__

Query dynamic types for equality.

## __Arguments__

  - __A__
    Shall be an object of extensible declared type or unlimited
    polymorphic.

  - __B__
    Shall be an object of extensible declared type or unlimited
    polymorphic.

## __Returns__

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of A is the same as the dynamic type of B.

## __Standard__

Fortran 2003 and later

## __See Also__

__extends\_type\_of__(3)
