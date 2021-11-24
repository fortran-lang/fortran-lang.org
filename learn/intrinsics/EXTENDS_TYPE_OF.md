---
layout: book
title: extends_type_of
permalink: /learn/intrinsics/EXTENDS_TYPE_OF
---
### NAME

__extends\_type\_of__(3f) - \[\] determine if the dynamic type of A is an extension of the dynamic type of MOLD.
(GFDL)

### SYNOPSIS

RESULT=EXTENDS\_TYPE\_OF (*A*, *MOLD*)

### DESCRIPTION

__extends\_type\_of__(3f) is TRUE if and only if the dynamic type of *A*
is an extension of the dynamic type of *MOLD*.

### OPTIONS

  - __*A__*
    shall be an object of extensible type. If it is a pointer, it shall
    not have an undefined association status.

  - __*MOLD__*
    shall be an object of extensible type. If it is a pointer, it shall
    not have an undefined association status.

### RESULTS

  - __RESULT__
    Default logical scalar.

  - __VALUE__
    If *MOLD* is unlimited polymorphic and is either a disassociated
    pointer or unallocated allocatable variable, the result is true;
    otherwise if *A* is unlimited polymorphic and is either a
    disassociated pointer or unallocated allocatable variable, the
    result is false; otherwise the result is true if and only if the
    dynamic type of *A* is an extension type of the dynamic type of
    *MOLD*.

    The dynamic type of a disassociated pointer or unallocated
    allocatable variable is its declared type.

### EXAMPLE

### CLASS

Inquiry function.
