---
layout: book
title: same_type_as
permalink: /learn/intrinsics/SAME_TYPE_AS
---
### NAME

**same\_type\_as**(3f) - \[\] Query dynamic types for equality
(GFDL)

### SYNTAX

result = **same\_type\_as**(a, b)

### DESCRIPTION

Query dynamic types for equality.

### ARGUMENTS

  - **A**
    Shall be an object of extensible declared type or unlimited
    polymorphic.

  - **B**
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

**extends\_type\_of**(3)
