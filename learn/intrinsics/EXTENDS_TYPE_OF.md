---
layout: book
title: extends_type_of
permalink: /learn/intrinsics/EXTENDS_TYPE_OF
---
## __Name__

__extends\_type\_of__(3) - \[STATE\] determine if the dynamic type of __a__ is an extension of the dynamic type of __mold__.
(GFDL)

## __Syntax__
```fortran
result=extends_type_of(a, mold)
```
## __Description__

__extends\_type\_of__(3) is __.true.__ if and only if the dynamic type of __a__
is an extension of the dynamic type of __mold__.

## __Options__

  - __a__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

  - __mold__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

## __Returns__

  - __result__
    : Default logical scalar.

  - __value__
    : If __mold__ is unlimited polymorphic and is either a disassociated
    pointer or unallocated allocatable variable, the result is
    true; otherwise if __a__ is unlimited polymorphic and is either a
    disassociated pointer or unallocated allocatable variable, the result
    is false; otherwise the result is true if and only if the dynamic
    type of *A* is an extension type of the dynamic type of __mold__.

    The dynamic type of a disassociated pointer or unallocated
    allocatable variable is its declared type.

## __Examples__

###### fortran-lang intrinsic descriptions
