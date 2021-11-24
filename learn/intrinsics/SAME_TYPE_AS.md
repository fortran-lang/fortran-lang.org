---
layout: book
title: same_type_as
permalink: /learn/intrinsics/SAME_TYPE_AS
---
### NAME

__same\_type\_as__(3f) - \[\] Query dynamic types for equality
(GFDL)

### SYNTAX

result = __same\_type\_as__(a, b)

### DESCRIPTION

Query dynamic types for equality.

### ARGUMENTS

  - __A__
    Shall be an object of extensible declared type or unlimited
    polymorphic.

  - __B__
    Shall be an object of extensible declared type or unlimited
    polymorphic.

### RETURN VALUE

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of A is the same as the dynamic type of B.

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function

### SEE ALSO

__extends\_type\_of__(3)
